(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_TEXT_IO =
   MLTON_IO
   where type instream = TextIO.instream
   where type outstream = TextIO.outstream
