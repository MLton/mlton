(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
