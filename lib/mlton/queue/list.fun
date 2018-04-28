(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                             ListQueue                             *)
(*-------------------------------------------------------------------*)

functor ListQueue(): BASIC_PERSISTENT_QUEUE =
struct

structure L = List

datatype 'a t = T of 'a List.t

fun destruct(T l) =
   case L.destruct l of
      NONE => NONE
    | SOME(x, l) => SOME(x, T l)

fun empty () = T(L.empty())

fun isEmpty(T l) = L.isEmpty l

fun enque(T l, x) = T(L.append(l, L.single x))

end

structure ListQueue = PersistentQueue(ListQueue())
