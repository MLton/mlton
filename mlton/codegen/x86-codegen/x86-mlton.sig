(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_MLTON_BASIC_STRUCTS =
  sig
    structure x86 : X86_PSEUDO
    structure MachineOutput: MACHINE_OUTPUT
    sharing x86.Label = MachineOutput.Label
  end

signature X86_MLTON_BASIC =
  sig
    include X86_MLTON_BASIC_STRUCTS

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
    structure Classes :
      sig
	val Heap : x86.MemLoc.Class.t
	val Stack : x86.MemLoc.Class.t
	val Locals : x86.MemLoc.Class.t
	val Globals : x86.MemLoc.Class.t

	val Temp : x86.MemLoc.Class.t
	val CStack : x86.MemLoc.Class.t
	val Code : x86.MemLoc.Class.t

	val CStatic : x86.MemLoc.Class.t
	val StaticTemp : x86.MemLoc.Class.t
	val StaticNonTemp : x86.MemLoc.Class.t
	  
	val GCState : x86.MemLoc.Class.t
	val GCStateHold : x86.MemLoc.Class.t
	  
	val IntInfRes : x86.MemLoc.Class.t	  
	val ThreadStack : x86.MemLoc.Class.t
	  
	val allClasses : x86.ClassSet.t
	val livenessClasses : x86.ClassSet.t
	val cstaticClasses : x86.ClassSet.t
	val runtimeClasses : x86.ClassSet.t
	val holdClasses : x86.ClassSet.t
      end

    (*
     * Static memory locations
     *)
    val makeContents : {base: x86.Immediate.t,
			size: x86.Size.t,
			class: x86.MemLoc.Class.t} -> x86.MemLoc.t
    (* CStack locations *)
    val c_stackPContents : x86.MemLoc.t
    val c_stackPContentsOperand : x86.Operand.t
    val c_stackPDerefOperand : x86.Operand.t
    val c_stackPDerefDoubleOperand : x86.Operand.t

    (* Static temps defined in x86codegen.h *)
    val limitCheckTempContentsOperand : x86.Operand.t
    val arrayAllocateTempContents : x86.MemLoc.t
    val arrayAllocateTempContentsOperand : x86.Operand.t
    val arrayAllocateLoopTempContentsOperand : x86.Operand.t
    val arrayAllocateLoopTempDerefOperand : x86.Operand.t
    val applyFFTempContentsOperand : x86.Operand.t
    val overflowCheckTempContentsOperand : x86.Operand.t
    val threadTempContentsOperand : x86.Operand.t
    val fileTempContentsOperand : x86.Operand.t
    val realTemp1ContentsOperand : x86.Operand.t
    val realTemp2ContentsOperand : x86.Operand.t
    val realTemp3ContentsOperand : x86.Operand.t
    val fpswTempContentsOperand : x86.Operand.t
    val statusTempContentsOperand : x86.Operand.t
    val intInfTempContentsOperand : x86.Operand.t
    val intInfTempValueContentsOperand : x86.Operand.t
    val intInfTempFrontierContentsOperand : x86.Operand.t

    (* Static arrays defined in x86codegen.h *)
    val local_base : MachineOutput.Type.t -> x86.Label.t
    val global_base : MachineOutput.Type.t -> x86.Label.t
    val globalPointerNonRoot_base : x86.Label.t

    (* Static functions defined in x86codegen.h *)
    val saveGlobals : x86.Label.t
    val loadGlobals : x86.Label.t

    (* Misc. *)
    val fileNameLabel : x86.Label.t
    val fileName : x86.Operand.t
    val fileLine : unit -> x86.Operand.t

    (* gcState relative locations defined in gc.h *)
    val gcState_limitContents : x86.MemLoc.t
    val gcState_limitContentsOperand : x86.Operand.t
    val gcState_frontier : x86.Immediate.t
    val gcState_frontierContents : x86.MemLoc.t
    val gcState_frontierContentsOperand : x86.Operand.t
    val gcState_frontierDeref : x86.MemLoc.t
    val gcState_frontierDerefOperand : x86.Operand.t
    val gcState_currentThread : x86.Immediate.t
    val gcState_currentThreadContents : x86.MemLoc.t
    val gcState_currentThreadContentsOperand : x86.Operand.t
    val gcState_currentThread_exnStackContents : x86.MemLoc.t
    val gcState_currentThread_exnStackContentsOperand : x86.Operand.t
    val gcState_stackTop : x86.Immediate.t
    val gcState_stackTopContents : x86.MemLoc.t
    val gcState_stackTopContentsOperand : x86.Operand.t
    val gcState_stackTopDeref : x86.MemLoc.t
    val gcState_stackTopDerefOperand : x86.Operand.t
    val gcState_stackBottomContents : x86.MemLoc.t
    val gcState_stackBottomContentsOperand : x86.Operand.t
    val gcState_stackLimitContents : x86.MemLoc.t
    val gcState_stackLimitContentsOperand : x86.Operand.t

    (*
     * GC related constants and functions
     *)
    val gcState : x86.Label.t

    val GC_OBJECT_HEADER_SIZE : int
    val gcObjectHeader : {nonPointers: int, pointers: int} -> x86.Immediate.t
    val gcArrayHeader : {nonPointers: int, pointers: int} -> x86.Immediate.t
  end

signature X86_MLTON_STRUCTS =
  sig
    structure x86MLtonBasic : X86_MLTON_BASIC
    structure x86Liveness : X86_LIVENESS
    sharing x86MLtonBasic.x86 = x86Liveness.x86
  end

signature X86_MLTON =
  sig
    include X86_MLTON_STRUCTS
    include X86_MLTON_BASIC
    sharing x86 = x86MLtonBasic.x86
    sharing x86 = x86Liveness.x86
    sharing x86.Label = MachineOutput.Label

    val wordAlign : int -> int
    (* bug, runtime and primitive Assembly sequences. *)
    val bug : {liveInfo: x86Liveness.LiveInfo.t} -> x86.Block.t' AppendList.t
    val invokeRuntime : {target : x86.Label.t, 
			 args : (x86.Operand.t * x86.Size.t) list, 
			 info : {frameSize: int, 
				 live: x86.Operand.t list,
				 return: x86.Label.t},
			 frameLayouts : x86.Label.t
                                        -> {size: int, frameLayoutsIndex: int} option,
			 liveInfo : x86Liveness.LiveInfo.t}
                        -> x86.Block.t' AppendList.t

    structure PrimInfo :
      sig
	datatype t
	  = None
	  | Overflow of x86.Label.t * x86.Operand.t list
	  | Runtime of {frameSize: int, 
			live: x86.Operand.t list,
			return: x86.Label.t}
	  | Normal of x86.Operand.t list
      end

    val applyPrim : {oper : MachineOutput.Prim.t,
		     args : (x86.Operand.t * x86.Size.t) list,
		     dst : (x86.Operand.t * x86.Size.t) option,
		     pinfo : PrimInfo.t,	
		     frameLayouts : x86.Label.t
                                    -> {size: int, frameLayoutsIndex: int} option,
		     liveInfo : x86Liveness.LiveInfo.t}
                    -> x86.Block.t' AppendList.t
  end
