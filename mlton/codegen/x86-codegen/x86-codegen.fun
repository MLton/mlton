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
					    frameLayouts,
					    frameOffsets,
					    handlesSignals,
					    intInfs,
					    main,
					    maxFrameSize,
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

	val makeC = outputC
	val makeS = outputS

	(* C specific *)
	fun outputC ()
	  = let
	      val {file, print, done} = makeC ()
	      fun make (name, l, pr, last) =
		 (print (concat ["static ", name, " = {"])
		  ; List.foreachi (l, fn (i, x) =>
				   (if i > 0 then print "," else ()
				       ; print "\n\t"
				       ; pr x))
		  ; (case last of
			NONE => ()
		      | SOME s => print (concat [",\n\t", s, "\n"]))
		  ; print "};\n")
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
		 in
		    [mainLabel, if reserveEsp then C.truee else C.falsee]
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
		 declareLocals ()
	    in
	      CCodegen.outputDeclarations
	      {additionalMainArgs = additionalMainArgs,
	       includes = includes,
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

	fun frameInfoToX86 (Machine.FrameInfo.T {frameLayoutsIndex, ...}) =
	   x86.FrameInfo.T
	   {frameLayoutsIndex = frameLayoutsIndex,
	    size = #size (Vector.sub (frameLayouts, frameLayoutsIndex))}
	   
	fun outputChunk (chunk as Machine.Chunk.T {blocks, chunkLabel, ...},
			 print)
	  = let
	      val isMain 
		= Machine.ChunkLabel.equals(#chunkLabel main, chunkLabel)

	      val {chunk}
		= x86Translate.translateChunk 
		  {chunk = chunk,
		   frameInfoToX86 = frameInfoToX86,
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
			  of [] => done ()
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
