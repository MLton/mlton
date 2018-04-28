(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
