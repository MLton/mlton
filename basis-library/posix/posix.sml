(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
