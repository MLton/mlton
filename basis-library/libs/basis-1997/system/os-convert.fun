(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
