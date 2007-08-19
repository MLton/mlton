(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PosixTTYConvert
        (structure TTY: POSIX_TTY) :
        POSIX_TTY_1997 =
  struct
     open TTY
     structure I =
        struct
           open I
           structure Flags = FlagsConvert(structure Flags = I)
           open Flags
        end
     structure O =
        struct
           open O
           structure Flags = FlagsConvert(structure Flags = O)
           open Flags
        end
     structure C =
        struct
           open C
           structure Flags = FlagsConvert(structure Flags = C)
           open Flags
        end
     structure L =
        struct
           open L
           structure Flags = FlagsConvert(structure Flags = L)
           open Flags
        end
     open TC
  end
