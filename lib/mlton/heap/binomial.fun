(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor EagerBinomialHeap(S: HEAP_STRUCTS): HEAP =
struct

open S
   
structure Heap = ForestHeap(structure Key = Key)
open Heap

val new = newEager
   
val insert = insertEager
   
val decreaseKey = decreaseKeySift

val delete = deleteSift

val union = unionEager

end

functor LazyBinomialHeap(S: HEAP_STRUCTS): HEAP =
struct

open S

structure Heap = ForestHeap(structure Key = Key)
open Heap

val new = newLazy

val insert = insertLazy
   
val decreaseKey = decreaseKeySift

val delete = deleteSift

val union = unionLazy

end
