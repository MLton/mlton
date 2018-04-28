(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Q = QueueLogarithmicExplicit();
fun p q = Q.output(q, ", ", Int.output, Out.standard);
let val q = ref(Q.empty())
in Iterate.for(1, 20, fn n =>
               (p(!q) ;
                Out.newline Out.standard ;
                q := Q.enqueue(!q,n)))
end


structure Q = QueuePersistentTwoList

structure Q = Queue(QueuePersistentOrderOne())


val q = Q.empty(): int Q.t

Q.destruct q

Q.dequeue q

val q = Q.enqueue(Q.enqueue(q, 1), 2)

val (one, q') = Q.dequeue q

val (two, q'') = Q.dequeue q'

   Q.isEmpty q;
   Q.isEmpty q';
   Q.isEmpty q''

local
   functor Test = QueueTest (structure ListUtil = ListUtil)
      (structure Queue = QueuePersistentList) ;

   structure Ephemeral = Test(structure Queue' = QueueEphemeral)
   structure PersistentTwoList = Test(structure Queue' = QueuePersistentTwoList)
   structure OrderOne = Test(structure Queue' = QueuePersistentOrderOne)

   val numOps = 1000
in
   val _ = (Ephemeral.test numOps ;
            PersistentTwoList.test numOps ;
            OrderOne.test numOps)
end
