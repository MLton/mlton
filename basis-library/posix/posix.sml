(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Posix: POSIX_EXTRA =
   struct
      structure Error = PosixError

      structure Signal = PosixSignal

      structure Process = PosixProcess
	 
      structure ProcEnv = PosixProcEnv
      
      structure FileSys = PosixFileSys

      structure IO = PosixIO

      structure SysDB = PosixSysDB

      structure TTY = PosixTTY
   end
