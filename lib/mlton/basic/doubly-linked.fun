(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DoublyLinked(S: DOUBLY_LINKED_STRUCTS): DOUBLY_LINKED = 
struct

open S

fun prevp d = #1(destruct d)
fun prev d = Pointer.!(prevp d)
fun setPrev(d,d') = Pointer.:=(prevp d,d')
fun value d = #2(destruct d)
fun nextp d = #3(destruct d)
fun next d = Pointer.!(nextp d)
fun setNext(d,d') = Pointer.:=(nextp d,d')

fun link(d, d') =
   (setNext(d, d')
    ; setPrev(d',d))

fun insertL(d, d') =
   (if Pointer.isNull(prevp d') then () else link(prev d',d)
    ; link(d,d'))

fun insertR(d, d') =
   (if Pointer.isNull(nextp d) then () else link(d',next d)
    ; link(d, d'))

fun unlink d =
   (link(prev d,next d)
    ; Pointer.clear(prevp d)
    ; Pointer.clear(nextp d))

fun isLinked d =
   not(Pointer.isNull(prevp d) orelse Pointer.isNull(nextp d))

fun eqPrev(d, d') = Pointer.eq(prevp d, prevp d')

end
