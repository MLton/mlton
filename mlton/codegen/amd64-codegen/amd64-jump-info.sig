(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_JUMP_INFO_STRUCTS =
  sig
    structure amd64 : AMD64
  end

signature AMD64_JUMP_INFO =
  sig
    include AMD64_JUMP_INFO_STRUCTS

    datatype status = Count of int | None
    type t

    val newJumpInfo : unit -> t

    val completeJumpInfo : {chunk: amd64.Chunk.t,
                            jumpInfo: t} -> unit
    val completeJumpInfo_msg : unit -> unit
    val verifyJumpInfo : {chunk: amd64.Chunk.t,
                          jumpInfo: t} -> bool
    val verifyJumpInfo_msg : unit -> unit

    val incNear : t * amd64.Label.t -> unit
    val decNear : t * amd64.Label.t -> unit
    val getNear : t * amd64.Label.t -> status
  end
