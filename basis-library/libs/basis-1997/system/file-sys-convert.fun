(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
