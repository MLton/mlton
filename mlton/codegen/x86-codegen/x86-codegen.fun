(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor x86Codegen(S: X86_CODEGEN_STRUCTS): X86_CODEGEN =
struct
  open S

  structure x86 
    = x86(structure Label = Machine.Label
	  structure Runtime = Machine.Runtime)

  structure x86MLtonBasic
    = x86MLtonBasic(structure x86 = x86
		    structure Machine = Machine)

  structure x86Liveness
    = x86Liveness(structure x86 = x86
		  structure x86MLtonBasic = x86MLtonBasic)

  structure x86JumpInfo
    = x86JumpInfo(structure x86 = x86)

  structure x86LoopInfo
    = x86LoopInfo(structure x86 = x86)

  structure x86EntryTransfer
    = x86EntryTransfer(structure x86 = x86)

  structure x86MLton 
    = x86MLton(structure x86MLtonBasic = x86MLtonBasic
	       structure x86Liveness = x86Liveness)

  structure x86Translate 
    = x86Translate(structure x86 = x86
		   structure x86MLton = x86MLton
		   structure x86Liveness = x86Liveness)

  structure x86Simplify
    = x86Simplify(structure x86 = x86
		  structure x86Liveness = x86Liveness
		  structure x86JumpInfo = x86JumpInfo
		  structure x86EntryTransfer = x86EntryTransfer)

  structure x86GenerateTransfers
    = x86GenerateTransfers(structure x86 = x86
			   structure x86MLton = x86MLton
			   structure x86Liveness = x86Liveness
			   structure x86JumpInfo = x86JumpInfo
			   structure x86LoopInfo = x86LoopInfo
			   structure x86EntryTransfer = x86EntryTransfer)

  structure x86AllocateRegisters
    = x86AllocateRegisters(structure x86 = x86
			   structure x86MLton = x86MLton)

  structure x86Validate
    = x86Validate(structure x86 = x86)

  structure C =
    struct
      val truee = "TRUE"
      val falsee = "FALSE"

      fun bool b = if b then truee else falsee

      fun int(n: int): string 
	= if n >= 0
            then Int.toString n
	    else if n = Int.minInt
		   then "(int)0x80000000" (* because of goofy gcc warning *)
		   else "-" ^ String.dropPrefix(Int.toString n, 1)
      (* This overflows on Int32.minInt: Int32.toString(~ n) *)
    end

  open x86
  structure Type = Machine.Type
  fun output {program as Machine.Program.T {chunks,
					    frameOffsets,
					    handlesSignals,
					    intInfs,
					    main,
					    maxFrameSize,
					    profileAllocLabels,
					    strings,
					    ...},
              includes: string list,
	      outputC,
	      outputS}: unit
    = let
	 val reserveEsp =
	    (* There is no sigaltstack on cygwin, we need to reserve %esp to
	     * hold the C stack pointer.  We need to do this even in programs
	     * that don't handle signals, since signals get used under the hood
	     * in Cygwin.
	     *)
	    case !Control.hostType of
	       Control.Cygwin => true
	     | Control.FreeBSD => false
	     | Control.Linux => false

	 val numProfileAllocLabels =
	    (* Add 1 for PROFILE_ALLOC_MISC *)
	    1 + Vector.length profileAllocLabels
	 val declareProfileAllocLabels =
	    if !Control.profile <> Control.ProfileAlloc
	       then fn _ => ()
	    else
		let  
		   val profileLabels =
		      Array.tabulate (numProfileAllocLabels, fn _ => NONE)
		   val labelSet: {done: bool ref,
				  hash: word,
				  index: int,
				  name: string} HashSet.t =
		      HashSet.new {hash = #hash}
		   val _ = 
		      Vector.foreachi (profileAllocLabels, fn (i, name) =>
				       let
					  val hash = String.hash name
				       in
					  HashSet.lookupOrInsert
					  (labelSet, hash, fn _ => false,
					   fn () => {done = ref false,
						     hash = hash,
						     index = i + 1,
						     name = name})
					  ; ()
				       end)
		   fun addProfileLabel (name: string, label: Label.t) =
		      case HashSet.peek (labelSet, String.hash name,
					 fn {name = n, ...} => n = name) of
			 NONE => ()
		       | SOME {done, index, ...} =>
			    if !done
			       then ()
			    else (done := true
				  ; Array.update (profileLabels, index,
						  SOME label))
		   val _ = x86.setAddProfileLabel addProfileLabel
		   fun declareLabels print =
		      let
			 val _ = print ".data\n\
	                               \.p2align 4\n\
				       \.global profileAllocLabels\n\
				       \profileAllocLabels:\n"
			 val _ =
			    Array.foreach
			    (profileLabels, fn l =>
			     (print
			      (concat
			       [".long ",
 				case l of
	 			   NONE => "0"
		 		 | SOME l => Label.toString l,
			       "\n"])))
		      in
			 ()
		      end
		in
		   declareLabels
		end

	val makeC = outputC
	val makeS = outputS

	val {get = getFrameLayoutIndex 
	         : Label.t -> {size: int, 
			       frameLayoutsIndex: int} option,
	     set = setFrameLayoutIndex, ...}
	  = Property.getSetOnce(Label.plist,
				Property.initConst NONE)

	val return_labels
	  = List.fold
	    (chunks,
	     [],
	     fn (Machine.Chunk.T {blocks, ...}, l)
	      => Vector.fold (blocks, l,
			      fn (Machine.Block.T {kind, label, ...}, l) =>
			      case Machine.Kind.frameInfoOpt kind of
				 NONE => l
			       | SOME fi => (label, fi) :: l))

	local
	  val hash' = fn {size, offsetIndex} => Word.fromInt (offsetIndex)
	  val hash = fn {size, offsetIndex, frameLayoutsIndex}
	              => hash' {size = size, offsetIndex = offsetIndex}

	  val table = HashSet.new {hash = hash}
	  val frameLayoutsData' = ref []
	  val maxFrameLayoutIndex' = ref 0
	in
	  val _
	    = List.foreach
	      (return_labels,
	       fn (label, Machine.FrameInfo.T {size, frameOffsetsIndex = offsetIndex})
	        => let
		     val info = {size = size, offsetIndex = offsetIndex}
		     val {frameLayoutsIndex, ...}
		       = HashSet.lookupOrInsert
		         (table,
			  hash' info,
			  fn {size = size', offsetIndex = offsetIndex', ...} => 
			  size = size' andalso offsetIndex = offsetIndex',
			  fn () => 
			  let
			    val _ = List.push(frameLayoutsData', info)
			    val frameLayoutsIndex = !maxFrameLayoutIndex'
			    val _ = Int.inc maxFrameLayoutIndex'
			  in
			    {size = size,
			     offsetIndex = offsetIndex,
			     frameLayoutsIndex = frameLayoutsIndex}
			  end)
		   in
		     setFrameLayoutIndex
		     (label,
		      SOME {size = size,
			    frameLayoutsIndex = frameLayoutsIndex})
		   end)
	  val frameLayoutsData = List.rev (!frameLayoutsData')
	  val maxFrameLayoutIndex = !maxFrameLayoutIndex'
	end

	(* C specific *)
	fun outputC ()
	  = let
	      val {file, print, done} = makeC ()
	      fun make(name, l, pr)
		= (print (concat["static ", name, " = {"]);
		   List.foreachi(l,
				 fn (i,x) => (if i > 0 then print "," else ();
					      pr x));
		   print "};\n");
	      fun declareFrameLayouts()
		= make("GC_frameLayout frameLayouts[]",
		       frameLayoutsData,
		       fn {size, offsetIndex}
		        => print (concat["\n\t{", 
					 C.int size, ",", 
					 "frameOffsets" ^ (C.int offsetIndex), 
					 "}"]))
	      val additionalMainArgs =
		 let
		    val mainLabel = Label.toString (#label main)
		    (* Drop the leading _ with Cygwin, because gcc will add it.
		     *)
		    val mainLabel =
		       case !Control.hostType of
			  Control.Cygwin => String.dropPrefix (mainLabel, 1)
			| Control.FreeBSD => mainLabel
			| Control.Linux => mainLabel
		    val (a1, a2, a3) =
		       if !Control.profile = Control.ProfileAlloc
			  then (C.bool true,
				"&profileAllocLabels",
				C.int numProfileAllocLabels)
		       else (C.bool false, C.int 0, C.int 0)
		 in
		    [mainLabel,
		     if reserveEsp then C.truee else C.falsee,
		     a1, a2, a3]
		 end
	      fun declareLocals () =
		 let
		    val tyMax =
		       Runtime.Type.memo
		       (fn t =>
			List.fold
			(chunks, ~1, fn (Machine.Chunk.T {regMax, ...}, max) =>
			 Int.max (max, regMax t)))
		 in
		    print
		    (concat ["Locals",
			     Layout.toString
			     (Layout.tuple (List.map
					    (Runtime.Type.all, fn t =>
					     Int.layout (1 + tyMax t)))),
			     ";\n"])
		 end
	      fun rest () =
		 (declareLocals ()
		  ; declareFrameLayouts ()
		  ; print "extern uint profileAllocLabels;\n")
	    in
	      CCodegen.outputDeclarations
	      {additionalMainArgs = additionalMainArgs,
	       includes = includes,
	       maxFrameIndex = maxFrameLayoutIndex,
	       name = "X86",
	       print = print,
	       program = program,
	       rest = rest}
	      ; done ()
	    end 

        val outputC = Control.trace (Control.Pass, "outputC") outputC

	(* Assembly specific *)

	val _ = x86MLtonBasic.init ()

	fun file_begin file
	  = [x86.Assembly.pseudoop_data (),
	     x86.Assembly.pseudoop_p2align 
	     (x86.Immediate.const_int 2, NONE, NONE),
	     x86.Assembly.label x86MLton.fileNameLabel,
	     x86.Assembly.pseudoop_string [file]]

	val liveInfo = x86Liveness.LiveInfo.newLiveInfo ()
	val jumpInfo = x86JumpInfo.newJumpInfo ()

	fun outputChunk (chunk as Machine.Chunk.T {blocks, chunkLabel, ...},
			 print)
	  = let
	      val isMain 
		= Machine.ChunkLabel.equals(#chunkLabel main, chunkLabel)

	      val {chunk}
		= x86Translate.translateChunk 
		  {chunk = chunk,
		   frameLayouts = getFrameLayoutIndex,
		   liveInfo = liveInfo}
		  handle exn
		   => Error.bug ("x86Translate.translateChunk::" ^ 
				 (case exn
				    of Fail s => s
				     | _ => "?"))
		  
	      val chunk : x86.Chunk.t
		= x86Simplify.simplify 
		  {chunk = chunk,
		   (* don't perform optimizations on
		    * the main function (initGlobals)
		    *)
		   optimize = if isMain
				then 0
				else !Control.Native.optimize,
		   liveInfo = liveInfo,
		   jumpInfo = jumpInfo}
		  handle exn
		   => Error.bug ("x86Simplify.simplify::" ^
				 (case exn
				    of Fail s => s
				     | _ => "?"))

	      val unallocated_assembly : x86.Assembly.t list list
		= (x86GenerateTransfers.generateTransfers
		   {chunk = chunk,
		    optimize = !Control.Native.optimize,
		    liveInfo = liveInfo,
		    jumpInfo = jumpInfo,
		    reserveEsp = reserveEsp})
		  handle exn
		   => (Error.bug ("x86GenerateTransfers.generateTransfers::" ^
				  Layout.toString (Exn.layout exn)))

	      val allocated_assembly : Assembly.t list list
		= x86AllocateRegisters.allocateRegisters 
		  {assembly = unallocated_assembly,
		   (* don't calculate liveness info
		    * on the main function (initGlobals)
		    *)
		   liveness = not isMain}
		  handle exn
		   => Error.bug ("x86AllocateRegister.allocateRegisters::" ^
				 (case exn
				    of Fail s => s
				     | _ => "?"))

	      val _ 
		= Assert.assert
		  ("x86CodeGen.output: invalid",
		   fn () => x86Validate.validate 
		            {assembly = allocated_assembly}
			    handle exn
			     => Error.bug ("x86Validate.validate::" ^ 
					   (case exn
					      of Fail s => s
					       | _ => "?")))

	      val validated_assembly = allocated_assembly

	      val _ = Vector.foreach (blocks, Label.clear o Machine.Block.label)
	      val _ = x86.Immediate.clearAll ()
	      val _ = x86.MemLoc.clearAll ()
	    in
	      List.fold
	      (validated_assembly,
	       0,
	       fn (block, n)
	        => List.fold
	           (block,
		    n,
		    fn (asm, n)
		     => (Layout.print (Assembly.layout asm, print);
			 print "\n";
			 n + 1)))
	    end
	  
	fun outputAssembly ()
	  = let
	      val split = !Control.Native.split
	      fun loop chunks
		= let
		    val {file, print, done} = makeS()
		    val _ = List.foreach
		            (file_begin file,
			     fn asm => (Layout.print(Assembly.layout asm, print);
					print "\n"))
		    fun loop' (chunks, size) 
		      = case chunks
			  of [] =>
			     (declareProfileAllocLabels print
			      ; done ())
			   | chunk::chunks
			   => if (case split
				    of NONE => false
				     | SOME maxSize => size > maxSize)
				then (done (); loop (chunk::chunks))
				else loop'(chunks, 
					   size + outputChunk (chunk, print))
		  in 
		    loop' (chunks, 0)
		  end
	    in 
	      loop chunks
	      ; x86Translate.translateChunk_totals ()
              ; x86Simplify.simplify_totals ()
              ; x86GenerateTransfers.generateTransfers_totals ()
	      ; x86AllocateRegisters.allocateRegisters_totals ()
	      ; x86Validate.validate_totals ()
	    end

	val outputAssembly =
	   Control.trace (Control.Pass, "outputAssembly") outputAssembly
      in
	outputC()
	; outputAssembly()
      end 
end
