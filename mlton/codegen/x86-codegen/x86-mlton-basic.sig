(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_MLTON_BASIC_STRUCTS =
  sig
    structure x86 : X86_PSEUDO
    structure Machine: MACHINE
    sharing x86.Label = Machine.Label
    sharing x86.Prim = Machine.Prim
  end

signature X86_MLTON_BASIC =
  sig
    include X86_MLTON_BASIC_STRUCTS

    val init : unit -> unit

    (*
     * x86.Size.t equivalents
     *)
    val wordSize : x86.Size.t
    val wordBytes : int
    val wordScale : x86.Scale.t
    val pointerSize : x86.Size.t
    val pointerBytes : int
    val pointerScale : x86.Scale.t
    val objectHeaderBytes : int
    val arrayHeaderBytes : int
    val intInfOverheadBytes : int

    val toX86Size : Machine.Type.t -> x86.Size.t
    val toX86Scale : Machine.Type.t -> x86.Scale.t

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
	  
	val allClasses : x86.ClassSet.t ref
	val livenessClasses : x86.ClassSet.t ref
	val holdClasses : x86.ClassSet.t ref
	val runtimeClasses : x86.ClassSet.t ref
	val heapClasses : x86.ClassSet.t ref
	val cstaticClasses : x86.ClassSet.t ref
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

    (* CReturn locations *)
    val cReturnTempContents : x86.Size.t -> x86.MemLoc.t
    val cReturnTempContentsOperand : x86.Size.t -> x86.Operand.t

    (* Static temps defined in x86codegen.h *)
    val applyFFTempContentsOperand : x86.Operand.t
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
    val local_base : Machine.Type.t -> x86.Label.t
    val global_base : Machine.Type.t -> x86.Label.t
    val globalPointerNonRoot_base : x86.Label.t

    (* Static functions defined in x86codegen.h *)
    val saveGlobals : x86.Label.t
    val loadGlobals : x86.Label.t

    (* Misc. *)
    val fileNameLabel : x86.Label.t
    val fileName : x86.Operand.t
    val fileLine : unit -> x86.Operand.t

    (* gcState relative locations defined in gc.h *)
    val gcState_baseContents: unit -> x86.MemLoc.t
    val gcState_baseContentsOperand: unit -> x86.Operand.t
    val gcState_canHandleContentsOperand: unit -> x86.Operand.t
    val gcState_currentThread: unit -> x86.Immediate.t
    val gcState_currentThreadContentsOperand: unit -> x86.Operand.t
    val gcState_currentThread_exnStackContents: unit -> x86.MemLoc.t
    val gcState_currentThread_exnStackContentsOperand: unit -> x86.Operand.t
    val gcState_currentThread_stackContentsOperand: unit -> x86.Operand.t
    val gcState_currentThread_stack_reservedContentsOperand: unit -> x86.Operand.t
    val gcState_currentThread_stack_usedContentsOperand: unit -> x86.Operand.t
    val gcState_fromSizeContentsOperand: unit -> x86.Operand.t
    val gcState_frontierContents: unit -> x86.MemLoc.t
    val gcState_frontierContentsOperand: unit -> x86.Operand.t
    val gcState_frontierDerefOperand: unit -> x86.Operand.t
    val gcState_limitContentsOperand: unit -> x86.Operand.t
    val gcState_limitPlusSlopContentsOperand: unit -> x86.Operand.t
    val gcState_maxFrameSizeContentsOperand: unit -> x86.Operand.t
    val gcState_signalIsPendingContentsOperand: unit -> x86.Operand.t
    val gcState_stackBottomContents: unit -> x86.MemLoc.t
    val gcState_stackBottomContentsOperand: unit -> x86.Operand.t
    val gcState_stackLimitContentsOperand: unit -> x86.Operand.t
    val gcState_stackTop: unit -> x86.Immediate.t
    val gcState_stackTopContents: unit -> x86.MemLoc.t
    val gcState_stackTopContentsOperand: unit -> x86.Operand.t
    val gcState_stackTopDerefOperand: unit -> x86.Operand.t
    val gcState_stackTopMinusWordDeref: unit -> x86.MemLoc.t
    val gcState_stackTopMinusWordDerefOperand: unit -> x86.Operand.t

    (*
     * GC related constants and functions
     *)
    val gcState : x86.Label.t

    val GC_OBJECT_HEADER_SIZE : int
    val gcObjectHeader : {nonPointers: int, pointers: int} -> x86.Immediate.t
    val gcArrayHeader : {nonPointers: int, pointers: int} -> x86.Immediate.t
  end
