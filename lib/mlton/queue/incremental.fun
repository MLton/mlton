(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                         IncrementalQueue                          *)
(*-------------------------------------------------------------------*)

(* Error. on Okasaki93 *)

(* reverses tail before it is needed
 and walks down list incrementally *)

functor IncrementalQueue(AR: APPEND_REVERSE)
  : BASIC_PERSISTENT_QUEUE =
struct

structure L = AR.L
open L.I

datatype 'a t = T of 'a AR.t * 'a AR.t * 'a L.t

fun tail l =
   case AR.destruct l of
      NONE => l
    | SOME(_, l) => l

fun queue(l, l', r) =
   if AR.length l >= L.length r then T(l, l', r)
   else let val l = AR.appendReverse(l, r)
        in T(l, l, L.empty())
        end

fun empty() = let val l = AR.empty()
              in T(l, l, L.empty())
              end

fun isEmpty(T(l, _, _)) = AR.isEmpty l

fun destruct(T(l, l', r)) =
   case AR.destruct l of
      NONE => NONE
    | SOME(x, l) => SOME(x, queue(l, l', r))

fun enque(T(l, l', r), x) = queue(l, l', L.cons(x, r))

end
