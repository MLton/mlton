(* scheduler.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* scheduler.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * This module implements the scheduling queues and preemption
 * mechanisms.
 *)

structure Scheduler : SCHEDULER =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure GlobalDebug = Debug
      structure Debug = LocalDebug(val debug = false)

      open Critical

      structure Q = ImpQueue 
      structure T = MLton.Thread
      structure TID = ThreadID
      structure SH = SchedulerHooks

      type thread_id = ThreadID.thread_id
      datatype thread = datatype RepTypes.thread
      datatype rdy_thread = datatype RepTypes.rdy_thread

      fun prep (THRD (tid, t)) = RTHRD (tid, T.prepare (t, ()))
      fun prepVal (THRD (tid, t), v) = RTHRD (tid, T.prepare (t, v))
      fun prepFn (THRD (tid, t), f) = RTHRD (tid, T.prepare (T.prepend (t, f), ()))

      (* the dummy thread Id; this is used when an ID is needed to get
       * the types right
       *)
      val dummyTid = TID.bogus "dummy"
      (* the error thread.  This thread is used to trap attempts to run CML
       * without proper initialization (i.e., via RunCML).  This thread is
       * enqueued by reset.
       *)
      val errorTid = TID.bogus "error"
      fun errorThrd () : unit thread =
         THRD (errorTid, T.new (fn () =>
               (GlobalDebug.sayDebug 
                ([fn () => "CML"], fn () => "**** Use RunCML.doit to run CML ****")
                ; raise Fail "CML not initialized")))

      local
         val curTid : thread_id ref = ref dummyTid
      in
         fun getThreadId (THRD (tid, _)) = tid
         fun getCurThreadId () = 
            let
               val tid = !curTid
            in
               tid
            end
         fun setCurThreadId tid = 
            let
               val () = Assert.assertAtomic' ("Scheduler.setCurThreadId", NONE)
            in 
               curTid := tid
            end
      end
      fun tidMsg () = TID.tidToString (getCurThreadId ())
      fun debug msg = Debug.sayDebug ([atomicMsg, tidMsg], msg)
      fun debug' msg = debug (fn () => msg)

      (* The thread ready queues:
       * rdyQ1 is the primary queue and rdyQ2 is the secondary queue.
       *)
      val rdyQ1 : rdy_thread Q.t = Q.new ()
      and rdyQ2 : rdy_thread Q.t = Q.new ()

      (* enqueue a thread in the primary queue *)
      fun enque1 thrd =
         (Assert.assertAtomic' ("Scheduler.enque1", NONE)
          ; Q.enque (rdyQ1, thrd))
      (* enqueue a thread in the secondary queue *)
      fun enque2 thrd =
         (Assert.assertAtomic' ("Scheduler.enque2", NONE)
          ; Q.enque (rdyQ2, thrd))
      (* dequeue a thread from the primary queue *)
      fun deque1 () =
         (Assert.assertAtomic' ("Scheduler.deque1", NONE)
          ; case Q.deque rdyQ1 of
               NONE => deque2 ()
             | SOME thrd => SOME thrd)
      (* dequeue a thread from the secondary queue *)
      and deque2 () =
         (Assert.assertAtomic' ("Scheduler.deque2", NONE)
          ; case Q.deque rdyQ2 of
               NONE => NONE
             | SOME thrd => SOME thrd)
      (* promote a thread from the secondary queue to the primary queue *)
      fun promote () =
         (Assert.assertAtomic' ("Scheduler.promote", NONE)
          ; case deque2 () of
               NONE => ()
             | SOME thrd => enque1 thrd)

      fun next () =
         let
            val () = Assert.assertAtomic' ("Scheduler.next", NONE)
            val thrd =
               case deque1 () of
                  NONE => !SH.pauseHook ()
                | SOME thrd => thrd
         in
            thrd
         end
      fun ready thrd = 
         let
            val () = Assert.assertAtomic' ("Scheduler.ready", NONE)
            val () = enque1 thrd
         in
            ()
         end
      local
         fun atomicSwitchAux msg f = 
            (Assert.assertAtomic (fn () => "Scheduler." ^ msg, NONE)
             ; T.atomicSwitch (fn t => 
                               let
                                  val tid = getCurThreadId ()
                                  val () = TID.mark tid
                                  val RTHRD (tid',t') = f (THRD (tid, t))
                                  val () = setCurThreadId tid'
                               in 
                                  t'
                               end))
      in
         fun atomicSwitch (f: 'a thread -> rdy_thread) =
            atomicSwitchAux "atomicSwitch" f
         fun switch (f: 'a thread -> rdy_thread) =
            (atomicBegin (); atomicSwitch f)
         fun atomicSwitchToNext (f: 'a thread -> unit) =
            atomicSwitchAux "atomicSwitchToNext" (fn thrd => (f thrd; next ()))
         fun switchToNext (f: 'a thread -> unit) =
            (atomicBegin (); atomicSwitchToNext f)
         fun atomicReadyAndSwitch (f: unit -> rdy_thread) =
            atomicSwitchAux "atomicReadyAndSwitch" (fn thrd => (ready (prep thrd); f ()))
         fun readyAndSwitch (f: unit -> rdy_thread) =
            (atomicBegin (); atomicReadyAndSwitch f)
         fun atomicReadyAndSwitchToNext (f: unit -> unit) =
            atomicSwitchAux "atomicReadyAndSwitchToNext" (fn thrd => (ready (prep thrd); f (); next ()))
         fun readyAndSwitchToNext (f: unit -> unit) =
            (atomicBegin (); atomicReadyAndSwitchToNext f)
      end

      fun new (f : thread_id -> ('a -> unit)) : 'a thread =
         let
            val () = Assert.assertAtomic' ("Scheduler.new", NONE)
            val tid = TID.new ()
            val t = T.new (f tid)
         in
            THRD (tid, t)
         end

      fun prepend (thrd : 'a thread, f : 'b -> 'a) : 'b thread =
         let
            val () = Assert.assertAtomic' ("Scheduler.prepend", NONE)
            val THRD (tid, t) = thrd
            val t = T.prepend (t, f)
         in
            THRD (tid, t)
         end

      fun unwrap (f : rdy_thread -> rdy_thread) (t: T.Runnable.t) : T.Runnable.t =
         let
            val () = Assert.assertAtomic' ("Scheduler.unwrap", NONE)
            val tid = getCurThreadId ()
            val RTHRD (tid', t') = f (RTHRD (tid, t))
            val () = setCurThreadId tid'
         in
            t'
         end


      (* reset various pieces of state *)
      fun reset running = 
         (atomicBegin ()
          ; setCurThreadId dummyTid
          ; Q.reset rdyQ1; Q.reset rdyQ2
          ; if not running then ready (prep (errorThrd ())) else ()
          ; atomicEnd ())
      (* what to do at a preemption (with the current thread) *)
      fun preempt (thrd as RTHRD (tid, _)) =
         let
            val () = Assert.assertAtomic' ("Scheduler.preempt", NONE)
            val () = debug' "Scheduler.preempt" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Scheduler.preempt", SOME 1)
            val () =
               if TID.isMarked tid
                  then (TID.unmark tid
                        ; promote ()
                        ; enque1 thrd)
                  else enque2 thrd
         in
            ()
         end

      val _ = reset false
   end
