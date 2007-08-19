(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor OSPathConvert
        (structure Path : OS_PATH) :
        OS_PATH_1997 =
  struct
     open Path

     val mkAbsolute = fn (path, relativeTo) =>
       mkAbsolute {path = path, relativeTo = relativeTo}
     val mkRelative = fn (path, relativeTo) =>
       mkRelative {path = path, relativeTo = relativeTo}
  end
