(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature EXN =
   sig
      type t = exn

      exception Bind
      exception Match
      exception Overflow
      exception Subscript
      
      val history: t -> string list
      val name: t -> string
      val layout: t -> Layout.t
   end
