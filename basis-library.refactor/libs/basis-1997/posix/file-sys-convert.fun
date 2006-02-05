(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PosixFileSysConvert
        (structure FileSys: POSIX_FILE_SYS) :
        POSIX_FILE_SYS_1997 =
  struct
     open FileSys
     val readdir = fn d =>
       case readdir d of
         NONE => ""
       | SOME s => s
     structure S =
        struct
           open S
           structure Flags = FlagsConvert(structure Flags = S)
           open Flags
        end
     structure O =
        struct
           open O
           structure Flags = FlagsConvert(structure Flags = O)
           open Flags
        end
  end
