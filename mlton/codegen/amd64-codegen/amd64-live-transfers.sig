(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_LIVE_TRANSFERS_STRUCTS =
  sig
    structure amd64 : AMD64
    structure amd64Liveness : AMD64_LIVENESS
    sharing amd64 = amd64Liveness.amd64
    structure amd64JumpInfo : AMD64_JUMP_INFO
    sharing amd64 = amd64JumpInfo.amd64
    structure amd64LoopInfo : AMD64_LOOP_INFO
    sharing amd64 = amd64LoopInfo.amd64
  end

signature AMD64_LIVE_TRANSFERS =
  sig
    include AMD64_LIVE_TRANSFERS_STRUCTS

    type t

    val computeLiveTransfers : {chunk : amd64.Chunk.t,
                                transferRegs : amd64.Entry.t -> amd64.Register.t list,
                                transferXmmRegs : amd64.Entry.t -> amd64.XmmRegister.t list, 
                                liveInfo : amd64Liveness.LiveInfo.t,
                                jumpInfo : amd64JumpInfo.t,
                                loopInfo : amd64LoopInfo.t} -> t
    val computeLiveTransfers_totals : unit -> unit

    val getLiveTransfers : t * amd64.Label.t -> 
                           ((amd64.MemLoc.t * amd64.Register.t * bool) list *
                            (amd64.MemLoc.t * amd64.XmmRegister.t * bool) list)
    val setLiveTransfersEmpty : t * amd64.Label.t -> unit 
  end
