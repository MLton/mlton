(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_TRANSLATE_STRUCTS =
  sig
    structure amd64: AMD64_PSEUDO
    structure amd64MLton : AMD64_MLTON
    sharing amd64 = amd64MLton.amd64
    structure amd64Liveness : AMD64_LIVENESS
    sharing amd64 = amd64Liveness.amd64
    sharing amd64MLton.amd64Liveness = amd64Liveness 
  end

signature AMD64_TRANSLATE =
  sig
    include AMD64_TRANSLATE_STRUCTS

    val translateChunk : {chunk: amd64MLton.Machine.Chunk.t,
                          frameInfoToAMD64: (amd64MLton.Machine.FrameInfo.t
                                           -> amd64.FrameInfo.t),
                          liveInfo: amd64Liveness.LiveInfo.t}
                         -> {chunk: amd64.Chunk.t}

    val translateChunk_totals : unit -> unit
  end
