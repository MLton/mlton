(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_MLTON_STRUCTS =
  sig
    structure x86 : X86_PSEUDO
    structure MachineOutput: MACHINE_OUTPUT
    sharing x86.Label = MachineOutput.Label
  end

signature X86_MLTON =
  sig
    include X86_MLTON_STRUCTS

    (*
     * x86.Size.t equivalents
     *)
    val wordSize : x86.Size.t
    val wordScale : x86.Scale.t
    val pointerSize : x86.Size.t
    val pointerScale : x86.Scale.t
    val toX86Size : MachineOutput.Type.t -> x86.Size.t
    val toX86Scale : MachineOutput.Type.t -> x86.Scale.t

    (*
     * Memory classes
     *)
    val Stack : x86.MemLoc.Class.t
    val Heap : x86.MemLoc.Class.t
    val Runtime : x86.MemLoc.Class.t
    val CStack : x86.MemLoc.Class.t

    (*
     * Static memory locations
     *)
    val limitCheckTempContents : x86.MemLoc.t
    val arrayAllocateTempContents : x86.MemLoc.t
    val arrayAllocateLoopTempContents : x86.MemLoc.t
    val arrayAllocateLoopTempDeref : x86.MemLoc.t
    val applyFFTempContents : x86.MemLoc.t

    val local_base : MachineOutput.Type.t -> x86.Label.t
    val global_base : MachineOutput.Type.t -> x86.Label.t
    val globalPointerNonRoot_base : x86.Label.t

    val gcState_limitContents : x86.MemLoc.t
    val gcState_frontier : x86.Immediate.t
    val gcState_frontierContents : x86.MemLoc.t
    val gcState_frontierDeref : x86.MemLoc.t
    val gcState_currentThread_exnStackContents : x86.MemLoc.t
    val gcState_stackTop : x86.Immediate.t
    val gcState_stackTopContents : x86.MemLoc.t
    val gcState_stackTopDeref : x86.MemLoc.t
    val gcState_stackBottomContents : x86.MemLoc.t
    val gcState_stackLimitContents : x86.MemLoc.t


    (*
     * GC related constants and functions
     *)
    val mltonState : x86.Label.t
    val gcState : x86.Label.t
    val saveGlobals : x86.Label.t
    val loadGlobals : x86.Label.t
    val fileNameLabel : x86.Label.t
    val fileName : x86.Operand.t
    val fileLine : unit -> x86.Operand.t

    val GC_OBJECT_HEADER_SIZE : int
    val gcObjectHeader : {nonPointers: int, pointers: int} -> x86.Immediate.t
    val gcArrayHeader : {nonPointers: int, pointers: int} -> x86.Immediate.t
    val wordAlign : int -> int

    (* bug, runtime and primitive Assembly sequences. *)
    val bug : x86.Block.t' list
    val invokeRuntime : {label: x86.Label.t, 
			 args: (x86.Operand.t * x86.Size.t) list, 
			 frameSize: int, 
			 return: x86.Label.t,
			 liveInfo : {get : x86.Label.t 
				           -> x86.MemLoc.t list,
				     set : x86.Label.t * 
				           x86.MemLoc.t list 
					   -> unit}}
                        -> x86.Block.t' list
    val applyPrim : {oper : MachineOutput.Prim.t,
		     args : (x86.Operand.t * x86.Size.t) list,
		     dst : (x86.Operand.t * x86.Size.t) option,
		     info : {frameSize : int,
			     return : x86.Label.t} option,
		     pinfo : MachineOutput.PrimInfo.t,
		     liveInfo : {get : x86.Label.t 
				       -> x86.MemLoc.t list,
				 set : x86.Label.t * x86.MemLoc.t list
				       -> unit}}
                    -> x86.Block.t' list
  end
