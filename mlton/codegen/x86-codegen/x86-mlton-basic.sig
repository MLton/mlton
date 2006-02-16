(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature X86_MLTON_BASIC_STRUCTS =
  sig
    structure Machine: MACHINE
    structure x86: X86_PSEUDO
    sharing x86.CFunction = Machine.CFunction
    sharing x86.CType = Machine.CType
    sharing x86.Label = Machine.Label
    sharing x86.ProfileLabel = Machine.ProfileLabel
    sharing x86.RepType = Machine.Type
    sharing x86.Runtime = Machine.Runtime
  end

signature X86_MLTON_BASIC =
  sig
    include X86_MLTON_BASIC_STRUCTS

    structure CFunction: C_FUNCTION
    structure CType: C_TYPE
    structure RepType: REP_TYPE
    sharing CFunction = RepType.CFunction
    sharing CType = RepType.CType
    sharing RepType = Machine.Type

    val init : unit -> unit

    (*
     * x86.Size.t equivalents
     *)
    val wordBytes : int
    val wordSize : x86.Size.t
    val wordScale : x86.Scale.t
    val pointerBytes : int
    val pointerSize : x86.Size.t
    val pointerScale : x86.Scale.t
    val normalHeaderBytes : int
    val arrayHeaderBytes : int
    val intInfOverheadBytes : int

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
        val StaticTemp : x86.MemLoc.Class.t
        val CStack : x86.MemLoc.Class.t
        val Code : x86.MemLoc.Class.t

        val CStatic : x86.MemLoc.Class.t
        val StaticNonTemp : x86.MemLoc.Class.t
          
        val GCState : x86.MemLoc.Class.t
        val GCStateHold : x86.MemLoc.Class.t
        val GCStateVolatile : x86.MemLoc.Class.t
          
        val allClasses : x86.ClassSet.t ref
        val livenessClasses : x86.ClassSet.t ref
        val holdClasses : x86.ClassSet.t ref
        val volatileClasses : x86.ClassSet.t ref
        val runtimeClasses : x86.ClassSet.t ref
        val heapClasses : x86.ClassSet.t ref
        val cstaticClasses : x86.ClassSet.t ref
      end

    (* CStack locations *)
    val c_stackPContents : x86.MemLoc.t
    val c_stackPContentsOperand : x86.Operand.t
    val c_stackPDerefOperand : x86.Operand.t
    val c_stackPDerefDoubleOperand : x86.Operand.t
    val c_stackPDerefFloatOperand : x86.Operand.t

    (* Static temps defined in x86-main.h *)
    val applyFFTempContentsOperand : x86.Operand.t
    val applyFFTemp2ContentsOperand : x86.Operand.t
    val threadTempContentsOperand : x86.Operand.t
    val fileTempContentsOperand : x86.Operand.t
    val realTemp1ContentsOperand : x86.Size.t -> x86.Operand.t
    val realTemp2ContentsOperand : x86.Size.t -> x86.Operand.t
    val realTemp3ContentsOperand : x86.Size.t -> x86.Operand.t
    val fildTempContentsOperand : x86.Operand.t
    val fpswTempContentsOperand : x86.Operand.t
    val statusTempContentsOperand : x86.Operand.t
    val eq1TempContentsOperand : x86.Operand.t
    val eq2TempContentsOperand : x86.Operand.t
    val wordTemp1ContentsOperand : x86.Size.t -> x86.Operand.t
    val wordTemp2ContentsOperand : x86.Size.t -> x86.Operand.t

    (* Static arrays defined in main.h and x86-main.h *)
    val local_base : x86.CType.t -> x86.Label.t
    val global_base : x86.CType.t -> x86.Label.t
    val globalPointerNonRoot_base : x86.Label.t

    (* Static functions defined in main.h *)
    val saveGlobals : x86.Label.t
    val loadGlobals : x86.Label.t

    (* Misc. *)
    val fileNameLabel : x86.Label.t
    val fileName : x86.Operand.t
    val fileLine : unit -> x86.Operand.t

    (* gcState relative locations defined in gc.h *)
    val gcState_label: x86.Label.t
    val gcState_offset: {offset: int, ty: x86.CType.t} -> x86.Operand.t
    val gcState_exnStackContents: unit -> x86.MemLoc.t
    val gcState_exnStackContentsOperand: unit -> x86.Operand.t
    val gcState_frontierContents: unit -> x86.MemLoc.t
    val gcState_frontierContentsOperand: unit -> x86.Operand.t
    val gcState_frontierDerefOperand: unit -> x86.Operand.t
    val gcState_stackBottomContents: unit -> x86.MemLoc.t
    val gcState_stackBottomContentsOperand: unit -> x86.Operand.t
    val gcState_stackTopContents: unit -> x86.MemLoc.t
    val gcState_stackTopContentsOperand: unit -> x86.Operand.t
    val gcState_stackTopDerefOperand: unit -> x86.Operand.t
    val gcState_stackTopMinusWordDeref: unit -> x86.MemLoc.t
    val gcState_stackTopMinusWordDerefOperand: unit -> x86.Operand.t

    val stackTopTempContentsOperand: unit -> x86.Operand.t
    val stackTopTempDerefOperand: unit -> x86.Operand.t
    val stackTopTempMinusWordDeref: unit -> x86.MemLoc.t
    val stackTopTempMinusWordDerefOperand: unit -> x86.Operand.t
  end
