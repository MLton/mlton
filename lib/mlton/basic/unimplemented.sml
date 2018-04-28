(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Unimplemented =
   struct
      val op equals = fn _ => Error.unimplemented "equals"
      fun layout _ = Error.unimplemented "layout"
      fun output _ = Error.unimplemented "output"
   end
