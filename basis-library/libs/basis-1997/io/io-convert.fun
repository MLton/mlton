(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor IOConvert
        (structure IO: IO) :
        IO_1997 =
  struct
     open IO
     exception TerminatedStream
  end