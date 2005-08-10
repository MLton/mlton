(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature X86_ENTRY_TRANSFER_STRUCTS =
  sig
    structure x86 : X86
  end

signature X86_ENTRY_TRANSFER =
  sig
    include X86_ENTRY_TRANSFER_STRUCTS

    val verifyEntryTransfer : {chunk: x86.Chunk.t} -> bool
    val verifyEntryTransfer_msg : unit -> unit
  end
