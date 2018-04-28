(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_LOOP_INFO_STRUCTS =
  sig
    structure x86 : X86
  end

signature X86_LOOP_INFO =
  sig
    include X86_LOOP_INFO_STRUCTS

    type t

    val createLoopInfo : {chunk: x86.Chunk.t, farLoops: bool} -> t
    val createLoopInfo_msg : unit -> unit

    val getLoopDistance : t * x86.Label.t * x86.Label.t -> int option
    val getLoopLabels : t * x86.Label.t -> x86.Label.t list
    val isLoopHeader : t * x86.Label.t -> bool
  end
