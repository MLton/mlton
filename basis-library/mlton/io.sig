(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_IO_ARG =
   sig
      type instream
      type outstream

      val inFd: instream -> Posix.IO.file_desc
      val newIn: Posix.IO.file_desc * string -> instream
      val newOut: Posix.IO.file_desc * string -> outstream
      val outFd: outstream -> Posix.IO.file_desc
   end

signature MLTON_IO =
   sig
      include MLTON_IO_ARG

      (* mkstemp s creates and opens a new temp file with prefix s, returning
       * the name of the temp file and the outstream to write to it.
       *)
      val mkstemp: string -> string * outstream
      (* mkstemps is like mkstemp, except it has both a prefix and suffix. *)
      val mkstemps: {prefix: string, suffix: string} -> string * outstream
      (* adds a suitable system or user specific prefix (dir) for temp files *)
      val tempPrefix : string -> string
   end
