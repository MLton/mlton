functor LazyAppendReverse(): APPEND_REVERSE =
struct
   
structure L' = LazyListLength(LazyList)

open L'

structure L = L'

end

structure LazyAppendReverse = LazyAppendReverse()
   
(* figure 1 of Okasaki96 *)
structure QueueAmortizedConst =
   PersistentQueue(EarlyQueue(LazyAppendReverse))
