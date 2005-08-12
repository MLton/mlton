(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PosixConvert
        (structure Posix : POSIX) :
        POSIX_1997 = 
  struct
     open Posix
     structure Process = PosixProcessConvert(structure Process = Process)
     structure FileSys = PosixFileSysConvert(structure FileSys = FileSys)
     structure IO = PosixIOConvert(structure IO = IO)
     structure TTY = PosixTTYConvert(structure TTY = TTY)
  end
