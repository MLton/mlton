(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_MLTON_BASIC_STRUCTS =
  sig
    structure Machine: MACHINE
    structure amd64: AMD64_PSEUDO
    sharing amd64.CFunction = Machine.CFunction
    sharing amd64.CType = Machine.CType
    sharing amd64.Label = Machine.Label
    sharing amd64.ProfileLabel = Machine.ProfileLabel
    sharing amd64.RepType = Machine.Type
    sharing amd64.Runtime = Machine.Runtime
    sharing amd64.WordSize = Machine.WordSize
    sharing amd64.WordX = Machine.WordX
  end

signature AMD64_MLTON_BASIC =
  sig
    include AMD64_MLTON_BASIC_STRUCTS

    structure CFunction: C_FUNCTION
    structure CType: C_TYPE
    structure RepType: REP_TYPE
    sharing CFunction = RepType.CFunction
    sharing CType = RepType.CType
    sharing RepType = Machine.Type

    val init : unit -> unit

    (*
     * amd64.Size.t equivalents
     *)
    val wordBytes : int
    val wordSize : amd64.Size.t
    val wordScale : amd64.Scale.t
    val pointerBytes : int
    val pointerSize : amd64.Size.t

    (*
     * Memory classes
     *)
    structure Classes :
      sig
        val Heap : amd64.MemLoc.Class.t
        val Stack : amd64.MemLoc.Class.t
        val Locals : amd64.MemLoc.Class.t
        val Globals : amd64.MemLoc.Class.t

        val Temp : amd64.MemLoc.Class.t
        val StaticTemp : amd64.MemLoc.Class.t
        val CArg : amd64.MemLoc.Class.t
        val CStack : amd64.MemLoc.Class.t
        val Code : amd64.MemLoc.Class.t

        val CStatic : amd64.MemLoc.Class.t
        val StaticNonTemp : amd64.MemLoc.Class.t

        val GCState : amd64.MemLoc.Class.t
        val GCStateHold : amd64.MemLoc.Class.t
        val GCStateVolatile : amd64.MemLoc.Class.t

        val allClasses : amd64.ClassSet.t ref
        val livenessClasses : amd64.ClassSet.t ref
        val holdClasses : amd64.ClassSet.t ref
        val volatileClasses : amd64.ClassSet.t ref
        val runtimeClasses : amd64.ClassSet.t ref
        val heapClasses : amd64.ClassSet.t ref
        val cstaticClasses : amd64.ClassSet.t ref
        val cargClasses : amd64.ClassSet.t ref
      end

    (* CStack locations *)
    val c_stackP : amd64.Label.t
    val c_stackPContents : amd64.MemLoc.t
    val c_stackPContentsOperand : amd64.Operand.t
    val c_stackPDerefWordOperand : amd64.Operand.t
    val c_stackPDerefDoubleOperand : amd64.Operand.t
    val c_stackPDerefFloatOperand : amd64.Operand.t

    (* Static temps defined in amd64-main.h *)
    val applyFFTempFunContentsOperand : amd64.Operand.t
    val applyFFTempRegArgContents : int -> amd64.MemLoc.t
    val applyFFTempXmmRegArgContents : amd64.Size.t * int -> amd64.MemLoc.t
    val fpcvtTempContentsOperand : amd64.Operand.t
    val fpeqTempContentsOperand : amd64.Size.t -> amd64.Operand.t

    (* Static arrays defined in main.h and amd64-main.h *)
    val local_base : amd64.CType.t -> amd64.Label.t
    val global_base : amd64.CType.t -> amd64.Label.t
    val globalObjptrNonRoot_base : amd64.Label.t

    (* gcState relative locations defined in gc.h *)
    val gcState_label: amd64.Label.t
    val gcState_offset: {offset: int, ty: amd64.CType.t} -> amd64.Operand.t
    val gcState_exnStackContents: unit -> amd64.MemLoc.t
    val gcState_exnStackContentsOperand: unit -> amd64.Operand.t
    val gcState_frontierContents: unit -> amd64.MemLoc.t
    val gcState_frontierContentsOperand: unit -> amd64.Operand.t
    val gcState_stackBottomContents: unit -> amd64.MemLoc.t
    val gcState_stackBottomContentsOperand: unit -> amd64.Operand.t
    val gcState_stackTopContents: unit -> amd64.MemLoc.t
    val gcState_stackTopContentsOperand: unit -> amd64.Operand.t
    val gcState_stackTopMinusWordDeref: unit -> amd64.MemLoc.t
    val gcState_stackTopMinusWordDerefOperand: unit -> amd64.Operand.t

    val stackTopTempContentsOperand: unit -> amd64.Operand.t
    val stackTopTempMinusWordDeref: unit -> amd64.MemLoc.t
    val stackTopTempMinusWordDerefOperand: unit -> amd64.Operand.t
  end
