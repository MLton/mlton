(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ARRAY_FINITE_FUNCTION =
   sig
      include FINITE_FUNCTION

      structure Domain :
         sig
            type 'a t = {size: int,
                         fromInt: int -> 'a,
                         toInt: 'a -> int}

            val pair: 'a1 t * 'a2 t
               * ('a1 -> 'a) * ('a2 -> 'a)
               * (('a * ('a1 -> int) * ('a2 -> int)) -> int)
               -> 'a t
         end

      val empty: 'a Domain.t -> ('a, 'b option) t
      val new: 'a Domain.t * 'b -> ('a, 'b) t
      val tabulate: 'a Domain.t * ('a -> 'b) -> ('a, 'b) t
      val set: ('a, 'b) t * 'a * 'b -> unit
   end
