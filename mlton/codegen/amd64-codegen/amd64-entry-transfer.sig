(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_ENTRY_TRANSFER_STRUCTS =
  sig
    structure amd64 : AMD64
  end

signature AMD64_ENTRY_TRANSFER =
  sig
    include AMD64_ENTRY_TRANSFER_STRUCTS

    val verifyEntryTransfer : {chunk: amd64.Chunk.t} -> bool
    val verifyEntryTransfer_msg : unit -> unit
  end
