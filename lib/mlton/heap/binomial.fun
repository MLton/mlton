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
