(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                          Queue Ephemeral                          *)
(*-------------------------------------------------------------------*)

functor QueueEphemeral(): BASIC_QUEUE =
struct

val {error, ...} = Error.errors("queue", "ephemeral")

structure L = MutableList

datatype 'a t = T of {head: 'a L.t ref,
                      tail: 'a L.t ref}

fun destruct(q as T{head, tail}) =
   case L.destruct(!head) of
      NONE => NONE
    | SOME(x, _) =>
         (if L.eqTail(!head, !tail)
             then (head := L.empty() ; tail := L.empty())
          else head := L.tail(!head) ;
             SOME(x, q))

fun empty() = T{head = ref(L.empty()), tail = ref(L.empty())}

fun isEmpty(T{head, ...}) = L.isEmpty(!head)

fun enque(q as T{head, tail}, x) =
    (let val cell = L.cons(x, L.empty())
     in (if isEmpty q then head := cell
         else L.setTail(!tail, cell) ;
         tail := cell)
     end ;
     q)

end
