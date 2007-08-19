(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FlagsConvert
        (structure Flags: BIT_FLAGS) :
        POSIX_FLAGS_1997 where type flags = Flags.flags =
  struct
     open Flags
     val wordTo = fromWord
  end
