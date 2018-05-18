(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                           TwoListQueue                            *)
(*-------------------------------------------------------------------*)

functor TwoListQueue (): BASIC_PERSISTENT_QUEUE =
struct

structure L = List

datatype 'a t = T of 'a L.t * 'a L.t

fun destruct(T(l, r)) =
   let val (l, r) = if L.isEmpty l
                      then (L.reverse r, L.empty())
                   else (l, r)
   in case L.destruct l of
      NONE => NONE
    | SOME(x, l) => SOME(x, T(l, r))
   end

fun empty() = T(L.empty(), L.empty())

fun isEmpty(T(l, r)) = L.isEmpty l andalso L.isEmpty r

fun enque(T(l, r), x) = T(l, L.cons(x, r))

fun toList(T(l, r)) = L.append(l, L.reverse r)

end

structure TwoListQueue = PersistentQueue(TwoListQueue())
