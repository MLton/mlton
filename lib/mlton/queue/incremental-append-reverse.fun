(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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
