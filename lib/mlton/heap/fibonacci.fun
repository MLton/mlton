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
