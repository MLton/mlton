(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure OS: OS =
   struct
      open PosixError

      structure FileSys = OS_FileSys
      structure Path = OS_Path
      structure Process = OS_Process
      structure IO = OS_IO
   end
