(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure OS =
   struct
      structure FileSys = OS_FileSys
      structure Path = OS_Path
      structure Process = OS_Process
      structure IO = OS_IO
      open PosixError
   end
