(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PosixProcessConvert
        (structure Process: POSIX_PROCESS) :
        POSIX_PROCESS_1997 =
  struct
     open Process
     structure W =
        struct
           open W
           structure Flags = FlagsConvert(structure Flags = W)
           open Flags
        end
  end
