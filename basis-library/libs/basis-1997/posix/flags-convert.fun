(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor FlagsConvert
        (structure Flags: BIT_FLAGS) :
        POSIX_FLAGS_1997 where type flags = Flags.flags =
  struct
     open Flags
     val wordTo = fromWord
  end