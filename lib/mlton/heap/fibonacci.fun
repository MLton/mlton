(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
