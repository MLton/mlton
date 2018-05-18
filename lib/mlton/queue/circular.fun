(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                           CircularQueue                           *)
(*-------------------------------------------------------------------*)

functor CircularQueue(): BOUNDED_EPHEMERAL_QUEUE =
struct

structure A = Array1D
structure I = A.I
open I

datatype 'a t = T of {size: I.t ref,
                      elts: 'a option A.t,
                      front: I.t ref,
                      back: I.t ref}

fun sizeRef(T{size=s, ...}) = s
fun incSize(T{size=s, ...}) = s := add1(!s)
fun decSize(T{size=s, ...}) = s := sub1(!s)
fun size d = !(sizeRef d)
fun elts(T{elts=e, ...}) = e
fun frontRef(T{front=f, ...}) = f
fun backRef(T{back=b, ...}) = b

fun maxSize d = A.size(elts d)

fun empty maxSize = T{size = ref zero,
                      elts = A.new(maxSize, NONE),
                      front = ref zero,
                      back = ref zero}

fun isEmpty d = isZero(size d)

fun isFull d = size d = maxSize d

fun inc(q, r) = let val r = r q
              in r := add1(!r) mod maxSize q
              end

fun incFront q = inc(q, frontRef)
fun incBack q = inc(q, backRef)

exception Enque
fun enque(q as T{size, elts, front, back}, x) =
   if isFull q then raise Enque
   else (if isEmpty q then (front := zero ; back := zero)
         else (incBack q ;
               incSize q ;
               A.update(elts, !back, SOME x)))

exception Deque
fun deque(q as T{size, elts, front, ...}) =
   if isEmpty q then raise Deque
   else case A.sub(elts, !front) of
           NONE => raise Deque
         | SOME x => (incFront q ;
                      decSize q ;
                      x)

end

structure CircularQueue = CircularQueue()
