(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor IntegralComparisons (type t
                             val < : t * t -> bool) =
   struct
      val < : t * t -> bool = <
      fun <= (a, b) = not (< (b, a))
      fun > (a, b) = < (b, a)
      fun >= (a, b) = <= (b, a)

      fun compare (i, j) =
         if < (i, j) then LESS
         else if < (j, i) then GREATER
         else EQUAL
      fun min (x, y) = if < (x, y) then x else y
      fun max (x, y) = if < (x, y) then y else x
   end
functor UnsignedIntegralComparisons (type int
                                     type word
                                     val idFromIntToWord : int -> word
                                     val < : word * word -> bool) =
   struct
      local
         fun ltu (i: int, i': int) = < (idFromIntToWord i, idFromIntToWord i')
         structure S = IntegralComparisons (type t = int 
                                            val < = ltu)
      in
         val ltu = S.<
         val leu = S.<=
         val gtu = S.>
         val geu = S.>=
      end
   end
