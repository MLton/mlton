(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                            EarlyQueue                             *)
(*-------------------------------------------------------------------*)

(* Error. on Okasaki93, Okasaki96 *)

(* reverses tail before it is needed *)

functor EarlyQueue(AR: APPEND_REVERSE): BASIC_PERSISTENT_QUEUE =
struct

structure L = AR.L
open L.I

datatype 'a t = T of 'a AR.t * 'a L.t

fun queue(l, r) =
   if AR.length l >= L.length r then T(l, r)
   else T(AR.appendReverse(l, r), L.empty())

fun empty() = T(AR.empty(), L.empty())

fun isEmpty(T(l, _)) = AR.isEmpty l

fun destruct(T(l, r)) =
   case AR.destruct l of
      NONE => NONE
    | SOME(x, l) => SOME(x, queue(l, r))

fun enque(T(l, r), x) = queue(l, L.cons(x, r))

end
