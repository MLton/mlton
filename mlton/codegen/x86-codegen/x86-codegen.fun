(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor x86Codegen (S: X86_CODEGEN_STRUCTS): X86_CODEGEN =
struct
  open S
     
  structure x86 = x86 (open Machine
		       structure RepType = Type)

  structure x86MLtonBasic
    = x86MLtonBasic (structure x86 = x86
		     structure Machine = Machine)

  structure x86Liveness
    = x86Liveness (structure x86 = x86
		   structure x86MLtonBasic = x86MLtonBasic)

  structure x86JumpInfo
    = x86JumpInfo (structure x86 = x86)

  structure x86LoopInfo
    = x86LoopInfo (structure x86 = x86)

  structure x86EntryTransfer
    = x86EntryTransfer (structure x86 = x86)

  structure x86MLton 
    = x86MLton (structure x86MLtonBasic = x86MLtonBasic
		structure x86Liveness = x86Liveness)

  val implementsPrim = x86MLton.implementsPrim
    
  structure x86Translate 
    = x86Translate (structure x86 = x86
		    structure x86MLton = x86MLton
		    structure x86Liveness = x86Liveness)

  structure x86Simplify
    = x86Simplify (structure x86 = x86
		   structure x86Liveness = x86Liveness
		   structure x86JumpInfo = x86JumpInfo
		   structure x86EntryTransfer = x86EntryTransfer)

  structure x86GenerateTransfers
    = x86GenerateTransfers (structure x86 = x86
			    structure x86MLton = x86MLton
			    structure x86Liveness = x86Liveness
			    structure x86JumpInfo = x86JumpInfo
			    structure x86LoopInfo = x86LoopInfo
			    structure x86EntryTransfer = x86EntryTransfer)

  structure x86AllocateRegisters
    = x86AllocateRegisters (structure x86 = x86
			    structure x86MLton = x86MLton)

  structure x86Validate
    = x86Validate (structure x86 = x86)

  structure C =
    struct
      val truee = "TRUE"
      val falsee = "FALSE"
    end

  open x86
  fun output {program as Machine.Program.T {chunks, frameLayouts, handlesSignals,
					    main, ...},
	      outputC: unit -> {file: File.t,
				print: string -> unit,
				done: unit -> unit},
	      outputS: unit -> {file: File.t,
				print: string -> unit,
				done: unit -> unit}}: unit
    = let
	 val reserveEsp =
	    (* There is no sigaltstack on cygwin, we need to reserve %esp to
	     * hold the C stack pointer.  We need to do this even in programs
	     * that don't handle signals, since signals get used under the hood
	     * in Cygwin.
	     *)
	    case !Control.reserveEsp of
	       NONE =>
		  handlesSignals
		  andalso let open Control in !targetOS = Cygwin end
	     | SOME b => b

	val makeC = outputC
	val makeS = outputS

	val Machine.Program.T {profileInfo, ...} = program
	val profileInfo =
	   case profileInfo of
	      NONE => Machine.ProfileInfo.empty
	    | SOME pi => pi
	val {newProfileLabel, delProfileLabel, getProfileInfo} = 
	  Machine.ProfileInfo.modify profileInfo

	(* C specific *)
	fun outputC ()
	  = let
	      local
		val Machine.Program.T 
		    {chunks, 
		     frameLayouts, 
		     frameOffsets, 
		     handlesSignals, 
		     intInfs, 
		     main, 
		     maxFrameSize, 
		     objectTypes, 
		     reals, 
		     strings, ...} =
		  program
	      in
		val program =
		  Machine.Program.T 
		  {chunks = chunks, 
		   frameLayouts = frameLayouts, 
		   frameOffsets = frameOffsets, 
		   handlesSignals = handlesSignals, 
		   intInfs = intInfs,  
		   main = main, 
		   maxFrameSize = maxFrameSize, 
		   objectTypes = objectTypes, 
		   profileInfo = SOME (getProfileInfo ()),
		   reals = reals, 
		   strings = strings} 
	      end
	      val {print, done, ...} = makeC ()
	      val additionalMainArgs =
		 let
		    val mainLabel = Label.toString (#label main)
		    (* Drop the leading _, because gcc will add it. *)
		    val mainLabel =
		       if !Control.labelsHaveExtra_
			  then String.dropPrefix (mainLabel, 1)
		       else mainLabel
		 in
		    [mainLabel, if reserveEsp then C.truee else C.falsee]
		 end
	      fun declareLocals () =
		 List.foreach
		 (CType.all,
		  fn t =>
		  let
		     val m =
			List.fold
			(chunks, ~1, fn (Machine.Chunk.T {regMax, ...}, max) =>
			 Int.max (max, regMax t))
		     val m = m + 1
		  in
		     print (concat [CType.toString t, 
				    " local", CType.toString t,
				    "[", Int.toString m, "];\n"])
		  end)
	      fun rest () =
		 declareLocals ()
	    in
	      CCodegen.outputDeclarations
	      {additionalMainArgs = additionalMainArgs,
	       includes = ["x86-main.h"],
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
	    size = Bytes.toInt (#size (Vector.sub (frameLayouts,
						   frameLayoutsIndex)))}
	   
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
		   delProfileLabel = delProfileLabel,
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
		    newProfileLabel = newProfileLabel,
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

	      val _ =
(*
		 Assert.assert
		 ("x86CodeGen.output: invalid",
		  fn () => 
*)
		  (ignore (x86Validate.validate 
			   {assembly = allocated_assembly}))
		  handle exn => 
		     Error.warning ("x86Validate.validate::" ^ 
				    (case exn of 
					Fail s => s
				      | _ => "?"))
(*
		 )
*)

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
	outputAssembly()
	; outputC()
      end 
end
