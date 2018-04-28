(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_SIMPLIFY_STRUCTS =
  sig
    structure x86 : X86
    structure x86Liveness : X86_LIVENESS
    sharing x86 = x86Liveness.x86
    structure x86JumpInfo : X86_JUMP_INFO
    sharing x86 = x86JumpInfo.x86
    structure x86EntryTransfer : X86_ENTRY_TRANSFER
    sharing x86 = x86EntryTransfer.x86
  end

signature X86_SIMPLIFY =
  sig
    include X86_SIMPLIFY_STRUCTS

    val simplify : {chunk : x86.Chunk.t,
                    optimize : int,
                    delProfileLabel : x86.ProfileLabel.t -> unit,
                    liveInfo : x86Liveness.LiveInfo.t,
                    jumpInfo : x86JumpInfo.t} -> x86.Chunk.t

    val simplify_totals : unit -> unit
  end
