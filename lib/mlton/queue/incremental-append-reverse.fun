functor IncrementalAppendReverse(): APPEND_REVERSE =
struct

structure L' = LazyListLength(LazyList)

open L'
   
val appendReverse = L'.appendReverseIncremental

structure L = L'
   
end

structure IncrementalAppendReverse = IncrementalAppendReverse()
   
(* figure 3 of Okasaki93 *)
structure QueueWorstCaseLog =
   PersistentQueue(EarlyQueue(IncrementalAppendReverse))

(* figure 4 of Okasaki93 *)
structure QueueWorstCaseConst =
   PersistentQueue(IncrementalQueue(IncrementalAppendReverse))
