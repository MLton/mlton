(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Unimplemented =
   struct
      val op equals = fn _ => Error.unimplemented "equals"
      fun layout _ = Error.unimplemented "layout"
      fun output _ = Error.unimplemented "output"
   end
