(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor OrderedRing(S: ORDERED_RING_STRUCTS):> ORDERED_RING where type t = S.t =
struct

open S
structure U = Ring(S)
open U

fun isPositive n = n > zero

fun isNegative n = n < zero

fun abs n = if isPositive n then n else ~ n

fun foldl(from, to, b, f) =
   let fun fold(n, a) = if n > to then a
                       else fold(add1 n, f(a,n))
   in fold(from, b)
   end

local 
   fun abs (combine, base) {from, to, term} =
      foldl(from, to, base, fn (a, i) => combine(a, term i))
in
   val sumFromTo = abs(op +, zero)
   val prodFromTo = abs(op *, one)
end

fun factorial n = prodFromTo{from = one, to = n, term = fn i => i}

fun max(m, n) = if m > n then m else n

fun min(m, n) = if m < n then m else n

end
