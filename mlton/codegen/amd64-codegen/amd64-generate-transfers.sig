(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_GENERATE_TRANSFERS_STRUCTS =
  sig
    structure amd64 : AMD64
    structure amd64MLton : AMD64_MLTON
    sharing amd64 = amd64MLton.amd64
    structure amd64Liveness : AMD64_LIVENESS
    sharing amd64 = amd64Liveness.amd64
    structure amd64JumpInfo : AMD64_JUMP_INFO
    sharing amd64 = amd64JumpInfo.amd64
    structure amd64LoopInfo : AMD64_LOOP_INFO
    sharing amd64 = amd64LoopInfo.amd64
    structure amd64EntryTransfer : AMD64_ENTRY_TRANSFER
    sharing amd64 = amd64EntryTransfer.amd64
  end

signature AMD64_GENERATE_TRANSFERS =
  sig
    include AMD64_GENERATE_TRANSFERS_STRUCTS

    val generateTransfers:
       {chunk: amd64.Chunk.t,
        optimize: int,
        newProfileLabel: amd64.ProfileLabel.t -> amd64.ProfileLabel.t,
        liveInfo: amd64Liveness.LiveInfo.t,
        jumpInfo: amd64JumpInfo.t,
        reserveRsp: bool} -> amd64.Assembly.t list list
    val generateTransfers_totals : unit -> unit
  end
