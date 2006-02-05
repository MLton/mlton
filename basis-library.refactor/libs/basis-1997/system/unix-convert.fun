(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor UnixConvert
        (structure Unix: UNIX) :
        UNIX_1997 =
  struct
     open Unix
     type proc = (TextIO.instream, TextIO.outstream) proc
  end
