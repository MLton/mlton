(* Copyright (C) 2022 Matthew Fluet.
 * Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_IO =
   sig
      type outstream

      (* mkstemp s creates and opens a new temp file with prefix s, returning
       * the name of the temp file and the outstream to write to it.
       *)
      val mkstemp: string -> string * outstream
      (* mkstemps is like mkstemp, except it has both a prefix and suffix. *)
      val mkstemps: {prefix: string, suffix: string} -> string * outstream
      (* adds a suitable system or user specific prefix (dir) for temp files *)
      val tempPrefix: string -> string
   end
