(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Posix =
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
