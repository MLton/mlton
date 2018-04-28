(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_LOOP_INFO_STRUCTS =
  sig
    structure amd64 : AMD64
  end

signature AMD64_LOOP_INFO =
  sig
    include AMD64_LOOP_INFO_STRUCTS

    type t

    val createLoopInfo : {chunk: amd64.Chunk.t, farLoops: bool} -> t
    val createLoopInfo_msg : unit -> unit

    val getLoopDistance : t * amd64.Label.t * amd64.Label.t -> int option
    val getLoopLabels : t * amd64.Label.t -> amd64.Label.t list
    val isLoopHeader : t * amd64.Label.t -> bool
  end
