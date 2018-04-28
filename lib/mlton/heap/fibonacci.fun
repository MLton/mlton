(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FibonacciHeap(S: HEAP_STRUCTS): HEAP =
struct

open S

structure Heap = ForestHeap(S)
open Heap

val new = newLazy

val insert = insertLazy

val decreaseKey = decreaseKeyCut

val delete = deleteCut

val union = unionLazy

end
