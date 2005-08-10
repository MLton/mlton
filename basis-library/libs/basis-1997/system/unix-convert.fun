(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor UnixConvert
        (structure Unix: UNIX) :
        UNIX_1997 =
  struct
     open Unix
     type proc = (TextIO.instream, TextIO.outstream) proc
  end
