(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor OSFileSysConvert
        (structure FileSys : OS_FILE_SYS) :
        OS_FILE_SYS_1997 =
  struct
     open FileSys
     val readDir = fn d =>
       case readDir d of
         NONE => ""
       | SOME s => s
  end
