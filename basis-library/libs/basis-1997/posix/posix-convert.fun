(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
