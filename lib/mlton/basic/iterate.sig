(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature ITERATE =
   sig
      val iterate: 'a * ('a -> bool) * ('a -> 'a) -> 'a
      (* iterate(s, p, f) = f(...f(f(s))) until satisfies p *)

      val whileDo: (unit -> bool) * (unit -> unit) -> unit

      val repeatUntil: (unit -> unit) * (unit -> bool) -> unit
   end
