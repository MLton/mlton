(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                          QueueLinkedList                          *)
(*-------------------------------------------------------------------*)

functor QueueLinkedList(): QUEUE_EPHEMERAL_UNBOUNDED =
struct

val {error, ...} = Error.errors("queue", "linked-list")

structure L = MutableList

datatype 'a t = T of {head: 'a L.t ref,
                      tail: 'a L.t ref}

fun empty() = T{head = ref(L.empty()), tail = ref(L.empty())}

fun isEmpty(T{head = ref l, ...}) = L.isEmpty l

fun enque(q as T{head, tail}, x) =
   let val cell = L.single x
   in (if isEmpty q then head := cell
       else L.setTail(!tail, cell) ;
       tail := cell)
   end

fun deque(q as T{head, tail}) =
   case L.destruct(!head) of
      NONE => error "deque"
    | SOME(x, _) => (if L.eq(!head, !tail)
                       then (head := L.empty() ; tail := L.empty())
                    else head := L.tail(!head) ;
                    x)

end
