(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_LIVE_TRANSFERS_STRUCTS =
  sig
    structure x86 : X86
    structure x86Liveness : X86_LIVENESS
    sharing x86 = x86Liveness.x86
    structure x86JumpInfo : X86_JUMP_INFO
    sharing x86 = x86JumpInfo.x86
    structure x86LoopInfo : X86_LOOP_INFO
    sharing x86 = x86LoopInfo.x86
  end

signature X86_LIVE_TRANSFERS =
  sig
    include X86_LIVE_TRANSFERS_STRUCTS

    type t

    val computeLiveTransfers : {chunk : x86.Chunk.t,
                                transferRegs : x86.Entry.t -> x86.Register.t list,
                                transferFltRegs : x86.Entry.t -> Int.t, 
                                liveInfo : x86Liveness.LiveInfo.t,
                                jumpInfo : x86JumpInfo.t,
                                loopInfo : x86LoopInfo.t} -> t
    val computeLiveTransfers_totals : unit -> unit

    val getLiveTransfers : t * x86.Label.t -> 
                           ((x86.MemLoc.t * x86.Register.t * bool) list *
                            (x86.MemLoc.t * bool) list)
    val setLiveTransfersEmpty : t * x86.Label.t -> unit 
  end
