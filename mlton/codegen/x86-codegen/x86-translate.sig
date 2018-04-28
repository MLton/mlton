(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_TRANSLATE_STRUCTS =
  sig
    structure x86: X86_PSEUDO
    structure x86MLton : X86_MLTON
    sharing x86 = x86MLton.x86
    structure x86Liveness : X86_LIVENESS
    sharing x86 = x86Liveness.x86
    sharing x86MLton.x86Liveness = x86Liveness 
  end

signature X86_TRANSLATE =
  sig
    include X86_TRANSLATE_STRUCTS

    val translateChunk : {chunk: x86MLton.Machine.Chunk.t,
                          frameInfoToX86: (x86MLton.Machine.FrameInfo.t
                                           -> x86.FrameInfo.t),
                          liveInfo: x86Liveness.LiveInfo.t}
                         -> {chunk: x86.Chunk.t}

    val translateChunk_totals : unit -> unit
  end
