(* mailbox.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* mailbox.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Asynchronous channels (called mailboxes).
 *)

structure Mailbox : MAILBOX_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure Q = FunQueue
      structure S = Scheduler
      structure E = Event
      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg)

      datatype trans_id = datatype TransID.trans_id
      datatype trans_id_state = datatype TransID.trans_id_state


      (* the state of a mailbox.  The queue of the NONEMPTY constructor should
       * never be empty (use EMPTY instead).
       *)
      datatype 'a state =
         EMPTY of (TransID.trans_id * 'a S.thread) Q.t
       | NONEMPTY of (int * 'a Q.t)

      datatype 'a mbox = MB of 'a state ref

      (*
      fun resetMbox (MB state) = state := EMPTY (Q.new ())
      *)

      fun mailbox () = MB (ref (EMPTY (Q.new ())))

      fun sameMailbox (MB s1, MB s2) = (s1 = s2)

      local
         fun cleaner (TXID txst, _) = 
            case !txst of CANCEL => true | _ => false
      in
         fun cleanAndDeque q =
            Q.cleanAndDeque (q, cleaner)
      end

      fun send (MB state, x) = 
         let
            val () = Assert.assertNonAtomic' "Mailbox.send"
            val () = debug' "Mailbox.send(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Mailbox.send(1)"
            val () = S.atomicBegin ()
            val () = debug' "Mailbox.send(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Mailbox.send(2)", SOME 1)
            val () = 
               case !state of 
                  EMPTY q => 
                     let
                        val () = debug' "Mailbox.send(3.1.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Mailbox.send(3.1.1)", SOME 1)
                     in
                        case (cleanAndDeque q) of
                           (NONE, _) => 
                              (let val q = Q.new ()
                               in state := NONEMPTY (1, Q.enque (q, x))
                               end
                               ; S.atomicEnd())
                         | (SOME (transId', t'), q') =>
                              S.readyAndSwitch
                              (fn () =>
                               (state := EMPTY q'
                                ; TransID.force transId'
                                ; S.prepVal (t', x)))
                     end
                | NONEMPTY (p, q) => 
                     (* we force a context switch here to prevent 
                      * a producer from outrunning a consumer.
                      *)
                     S.atomicReadyAndSwitchToNext
                     (fn () => state := NONEMPTY (p, Q.enque (q, x)))
            val () = debug' "Mailbox.send(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.send(4)"
         in
            ()
         end

      fun getMsg (state, q) = 
         let
            val (msg, q') = 
               case Q.deque q of
                  SOME (msg, q') => (msg, q')
                | NONE => raise Fail "Mailbox:getMsg"
            val () = if Q.empty q'
                        then state := EMPTY (Q.new ())
                        else state := NONEMPTY (1, q')
            val () = S.atomicEnd ()
         in
            msg
         end

      fun recv (MB state) = 
         let
            val () = Assert.assertNonAtomic' "Mailbox.recv"
            val () = debug' "Mailbox.recv(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Mailbox.recv(1)"
            val () = S.atomicBegin ()
            val () = debug' "Mailbox.recv(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Mailbox.recv(2)", SOME 1)
            val msg =
               case !state of 
                  EMPTY q => 
                     let
                        val msg = 
                           S.atomicSwitchToNext
                           (fn t => state := EMPTY (Q.enque (q, (TransID.mkTxId (), t))))
                     in
                        S.atomicEnd()
                        ; msg
                     end
                | NONEMPTY (_, q) => getMsg (state, q)
            val () = debug' "Mailbox.recv(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recv(4)"
         in
            msg
         end

      fun recvEvt (MB state) = 
         let
            fun blockFn {transId, cleanUp: unit -> unit, next} = 
               let
                  val q = 
                     case !state of
                        EMPTY q => q
                      | _ => raise Fail "Mailbox:recvEvt:blockFn"
                  val msg = 
                     S.atomicSwitch
                     (fn t => (state := EMPTY (Q.enque (q, (transId, t)))
                               ; next ()))
               in
                  cleanUp()
                  ; S.atomicEnd()
                  ; msg
               end
            fun pollFn () = 
               case !state of
                  EMPTY _ => E.blocked blockFn
                | NONEMPTY (prio, q) => 
                     (state := NONEMPTY (prio + 1, q)
                      ; E.enabled {prio = prio, 
                                   doitFn = fn () => getMsg (state, q)})
         in
            E.bevt pollFn
         end

      fun recvPoll (MB state) = 
         (S.atomicBegin()
          ; case !state of
               EMPTY _ => (S.atomicEnd(); NONE)
             | NONEMPTY (_, q) => SOME (getMsg (state, q)))
  end
