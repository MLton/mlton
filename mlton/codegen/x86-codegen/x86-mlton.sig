(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

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
    sharing x86.Label = Machine.Label
    sharing Machine = x86MLtonBasic.Machine

    type transInfo = {addData : x86.Assembly.t list -> unit,
                      frameInfoToX86: (x86MLtonBasic.Machine.FrameInfo.t
                                       -> x86.FrameInfo.t),
                      live: x86.Label.t -> x86.Operand.t list,
                      liveInfo: x86Liveness.LiveInfo.t}

    (* arith, c call, and primitive assembly sequences. *)
    val arith: {prim: RepType.t Machine.Prim.t,
                args: (x86.Operand.t * x86.Size.t) vector,
                dsts: (x86.Operand.t * x86.Size.t) vector,
                overflow: x86.Label.t,
                success: x86.Label.t,
                transInfo : transInfo} -> x86.Block.t' AppendList.t
    val ccall: {args: (x86.Operand.t * x86.Size.t) vector,
                frameInfo: x86.FrameInfo.t option,
                func: RepType.t Machine.CFunction.t,
                return: x86.Label.t option,
                transInfo: transInfo} -> x86.Block.t' AppendList.t
    val creturn: {dsts: (x86.Operand.t * x86.Size.t) vector,
                  frameInfo: x86.FrameInfo.t option,
                  func: RepType.t Machine.CFunction.t,
                  label: x86.Label.t, 
                  transInfo: transInfo} -> x86.Block.t' AppendList.t
  val implementsPrim: RepType.t Machine.Prim.t -> bool
  val prim: {prim: RepType.t Machine.Prim.t,
               args: (x86.Operand.t * x86.Size.t) vector,
               dsts: (x86.Operand.t * x86.Size.t) vector,
               transInfo: transInfo} -> x86.Block.t' AppendList.t
  end
