(* Copyright (C) 2002-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor OSConvert
        (structure OS: OS) :
        OS_1997 = 
  struct
     open OS
     structure FileSys = OSFileSysConvert(structure FileSys = FileSys)
     structure Path = OSPathConvert(structure Path = Path)
     structure Process = OSProcessConvert(structure Process = Process)
  end