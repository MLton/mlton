(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_GENERATE_TRANSFERS_STRUCTS =
  sig
    structure x86 : X86
    structure x86MLton : X86_MLTON
    sharing x86 = x86MLton.x86
    structure x86Liveness : X86_LIVENESS
    sharing x86 = x86Liveness.x86
    structure x86JumpInfo : X86_JUMP_INFO
    sharing x86 = x86JumpInfo.x86
    structure x86LoopInfo : X86_LOOP_INFO
    sharing x86 = x86LoopInfo.x86
    structure x86EntryTransfer : X86_ENTRY_TRANSFER
    sharing x86 = x86EntryTransfer.x86
  end

signature X86_GENERATE_TRANSFERS =
  sig
    include X86_GENERATE_TRANSFERS_STRUCTS

    val generateTransfers:
       {chunk: x86.Chunk.t,
        optimize: int,
        newProfileLabel: x86.ProfileLabel.t -> x86.ProfileLabel.t,
        liveInfo: x86Liveness.LiveInfo.t,
        jumpInfo: x86JumpInfo.t,
        reserveEsp: bool,
        picUsesEbx: bool} -> x86.Assembly.t list list
    val generateTransfers_totals : unit -> unit
  end
