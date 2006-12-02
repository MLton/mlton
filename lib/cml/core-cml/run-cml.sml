(* run-cml.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* run-cml-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure RunCML : RUN_CML =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure R = Running
      structure S = Scheduler
      structure SH = SchedulerHooks
      structure TID = ThreadID
      structure TO = TimeOut
      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg)


      local
         structure Signal = MLton.Signal
         structure Itimer = MLton.Itimer

         fun getAlrmHandler () =
            Signal.getHandler Posix.Signal.alrm
         fun setAlrmHandler h =
            Signal.setHandler (Posix.Signal.alrm, h)

         fun setItimer t =
            Itimer.set (Itimer.Real, {value = t, interval = t})
      in
         fun prepareAlrmHandler tq =
            let
               val origAlrmHandler = getAlrmHandler ()
               val tq = 
                  case tq of 
                     SOME tq => tq
                   | NONE => Time.fromMilliseconds 20
            in
               (fn alrmHandler =>
                (setAlrmHandler (Signal.Handler.handler (S.unwrap alrmHandler))
                 ; setItimer tq),
                fn () =>
                (setItimer Time.zeroTime
                 ; setAlrmHandler origAlrmHandler))
            end
      end

      fun isRunning () = !R.isRunning

      fun reset running =
         (S.reset running
          ; SH.reset ()
          ; TID.reset ()
          ; TO.reset ())

      fun alrmHandler thrd =
         let 
            val () = Assert.assertAtomic' ("RunCML.alrmHandler", NONE)
            val () = debug' "alrmHandler" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("RunCML.alrmHandler", SOME 1)
            val () = S.preempt thrd
            val () = ignore (TO.preempt ())
         in 
            S.next ()
         end

      (* Note that SH.pauseHook is only invoked by S.next
       * when there are no threads on the ready queue;
       * Furthermore, note that alrmHandler always
       * enqueues the preepted thread (via S.preempt).
       * Hence, the ready queue is never empty
       * at the S.next in alrmHandler.  Therefore,
       * pauseHook is never run within alrmHandler.
       *)
      fun pauseHook () =
         let
            val () = Assert.assertAtomic' ("RunCML.pauseHook", NONE)
            val () = debug' "pauseHook" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("RunCML.pauseHook", SOME 1)
            val to = TO.preempt ()
         in
            case to of
               NONE =>
                  (* no waiting threads *) 
                  S.prepFn (!SH.shutdownHook, fn () => (true, OS.Process.failure))
             | SOME NONE =>
                  (* enqueued a waiting thread *)
                  S.next ()
             | SOME (SOME t) => 
                  (* a waiting thread will be ready in t time *)
                  (if Time.toSeconds t <= 0
                      then ()
                      else S.doMasked (fn () => OS.Process.sleep t)
                   ; pauseHook ())
         end

      fun doit (initialProc: unit -> unit,
                tq: Time.time option) =
         let
            val () =
               if isRunning ()
                  then raise Fail "CML is running"
                  else ()
            val (installAlrmHandler, restoreAlrmHandler) = prepareAlrmHandler tq
            val ((*cleanUp*)_, status) =
               S.switchToNext
               (fn thrd => 
                let
                   val () = R.isRunning := true 
                   val () = reset true
                   val () = SH.shutdownHook := S.prepend (thrd, fn arg => (S.atomicBegin (); arg))
                   val () = SH.pauseHook := pauseHook
                   val () = installAlrmHandler alrmHandler
                   val () = ignore (Thread.spawn initialProc)
                in
                   ()
                end)
            val () = restoreAlrmHandler ()
            val () = reset false
            val () = R.isRunning := false
            val () = S.atomicEnd ()
         in
            status
         end

      fun shutdown status =
         if isRunning ()
            then S.switch (fn _ => S.prepVal (!SH.shutdownHook, (true, status)))
            else raise Fail "CML is not running"
   end
