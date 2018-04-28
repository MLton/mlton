(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

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
