(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(*-------------------------------------------------------------------*)
(*                           SinglyLinked                            *)
(*-------------------------------------------------------------------*)

functor SinglyLinkedQueue(): UNBOUNDED_EPHEMERAL_QUEUE = 
struct

structure P = Pointer
structure E = SimpleSinglyLinkedElement

datatype 'a t = T of {head: 'a E.t P.t,
		      tail: 'a E.t P.t}

fun empty() = T{head = P.null(), tail = P.null()}

fun isEmpty(T{head, ...}) = P.isNull head

fun enque(q as T{head, tail}, x) =
   let val e = E.new x
   in if isEmpty q then P.:=(head, e) else E.setNext(P.! tail, e) ;
      P.:=(tail, e)
   end

fun deque(T{head, tail}) =
   let val (v, p) = E.destruct(P.! head)
   in P.copy(head, p) ;
      if P.isNull p then P.clear tail else () ;
      v
   end

end

structure SinglyLinkedQueue = SinglyLinkedQueue()
