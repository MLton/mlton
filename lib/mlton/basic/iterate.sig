(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ITERATE =
   sig
      val iterate: 'a * ('a -> bool) * ('a -> 'a) -> 'a
      (* iterate(s, p, f) = f(...f(f(s))) until satisfies p *)

      val whileDo: (unit -> bool) * (unit -> unit) -> unit

      val repeatUntil: (unit -> unit) * (unit -> bool) -> unit
   end
