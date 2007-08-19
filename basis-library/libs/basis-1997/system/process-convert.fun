(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor OSProcessConvert
        (structure Process : OS_PROCESS) :
        OS_PROCESS_1997 =
  struct
     open Process
  end
