(* channel.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* channel.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of synchronous channels.
 *
 * To ensure that we always leave the atomic region exactly once, we
 * require that the blocking operation be responsible for leaving the
 * atomic region (in the event case, it must also execute the clean-up
 * action).  The doitFn always transfers control to the blocked thread
 * without leaving the atomic region.  Note that the send (and sendEvt)
 * blockFns run using the receiver's thread ID.
 *)

structure Channel : CHANNEL_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure Q = ImpQueue
      structure S = Scheduler
      structure E = Event
      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg)

      datatype trans_id = datatype TransID.trans_id
      datatype trans_id_state = datatype TransID.trans_id_state


      datatype 'a chan = 
         CHAN of {prio : int ref,
                  inQ  : (trans_id * 'a S.thread) Q.t,
                  outQ : (trans_id * 'a S.thread S.thread) Q.t}

      (*
      fun resetChan (CHAN {prio, inQ, outQ}) = 
         (prio := 1
          ; Q.reset inQ
          ; Q.reset outQ)
      *)

      fun channel () = CHAN {prio = ref 1, inQ = Q.new (), outQ = Q.new ()}

      (* sameChannel : ('a chan * 'a chan) -> bool *)
      fun sameChannel (CHAN {prio = prio1, ...}, CHAN {prio = prio2, ...}) =
         prio1 = prio2


      (* bump a priority value by one, returning the old value *)
      fun bumpPriority (p as ref n) = (p := n+1; n)

      (* functions to clean channel input and output queues *)
      local
         fun cleaner (TXID txst, _) = 
            case !txst of CANCEL => true | _ => false
      in
         fun cleanAndChk (prio, q) : int =
            (Q.clean (q, cleaner)
             ; if Q.empty q
                  then 0
                  else bumpPriority prio)
         fun cleanAndDeque q =
            Q.cleanAndDeque (q, cleaner)
         fun enqueAndClean (q, item) =
            Q.enqueAndClean (q, item, cleaner)
      end

      fun send (CHAN {prio, inQ, outQ}, msg) = 
         let
            val () = Assert.assertNonAtomic' "Channel.send"
            val () = debug' "Chennel.send(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.send(1)"
            val () = S.atomicBegin ()
            val () = debug' "Channel.send(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.send(2)", SOME 1)
            val () = 
               case cleanAndDeque inQ of 
                  SOME (rtxid, rt) => 
                     let
                        val () = debug' "Channel.send(3.1.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.send(3.1.1)", SOME 1)
                        val () =
                           S.readyAndSwitch
                           (fn () =>
                            (prio := 1
                             ; TransID.force rtxid
                             ; S.prepVal (rt, msg)))
                        val () = debug' "Channel.send(3.1.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.send(3.1.2)"
                     in
                        ()
                     end
                | NONE => 
                     let
                        val () = debug' "Channel.send(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.send(3.2.1)", SOME 1)
                        val rt = 
                           S.atomicSwitchToNext
                           (fn st => Q.enque (outQ, (TransID.mkTxId (), st)))
                        val () = debug' "Channel.send(3.2.2)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.send(3.2.2)", SOME 1)
                        val () = S.atomicReadyAndSwitch (fn () => S.prepVal (rt, msg))
                        val () = debug' "Chanell.send(3.2.3)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.send(3.2.2)"
                     in
                        ()
                     end
            val () = debug' "Channel.send(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.send(4)"
         in
            ()
         end

      fun sendEvt (CHAN {prio, inQ, outQ}, msg) =
         let
            fun doitFn () = 
               let
                  val () = Assert.assertAtomic' ("Channel.sendEvt.doitFn", NONE)
                  val (rtxid, rt) = valOf (Q.deque inQ)
                  val () = debug' "Channel.sendEvt(3.1.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.sendEvt(3.1.1)", SOME 1)
                  val () =
                     S.readyAndSwitch
                     (fn () =>
                      (prio := 1
                       ; TransID.force rtxid
                       ; S.prepVal (rt, msg)))
                  val () = debug' "Channel.sendEvt(3.1.2)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Channel.sendEvt(3.1.2)"
               in
                  ()
               end
            fun blockFn {transId, cleanUp, next} = 
               let
                  val () = Assert.assertAtomic' ("Channel.sendEvt.blockFn", NONE)
                  val () = debug' "Channel.sendEvt(3.2.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.sendEvt(3.2.1)", SOME 1)
                  val rt = 
                     S.atomicSwitch
                     (fn st =>
                      (enqueAndClean (outQ, (transId, st))
                       ; next ()))
                  val () = debug' "Channel.sendEvt(3.2.2)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.sendEvt(3.2.2)", SOME 1)
                  val () = cleanUp ()
                  val () = S.atomicReadyAndSwitch (fn () => S.prepVal (rt, msg))
                  val () = debug' "Channel.sendEvt(3.2.3)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Channel.sendEvt(3.2.2)"
               in
                  ()
               end
          fun pollFn () = 
             let
                val () = Assert.assertAtomic' ("Channel.sendEvt.pollFn", NONE)
                val () = debug' "Channel.sendEvt(2)" (* Atomic 1 *)
                val () = Assert.assertAtomic' ("Channel.sendEvt(2)", SOME 1)
             in
                case cleanAndChk (prio, inQ) of
                   0 => E.blocked blockFn
                 | prio => E.enabled {prio = prio, doitFn = doitFn}
             end
         in
            E.bevt pollFn
         end

      fun sendPoll (CHAN {prio, inQ, ...}, msg) = 
         let
            val () = Assert.assertNonAtomic' "Channel.sendPoll"
            val () = debug' "Channel.sendPoll(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.sendPoll(1)"
            val () = S.atomicBegin ()
            val () = debug' "Channel.sendPoll(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.sendPoll(1)", SOME 1)
            val b = 
               case cleanAndDeque inQ of 
                  SOME (rtxid, rt) => 
                     let
                        val () = debug' "Channel.sendPoll(3.1.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.sendPoll(3.1.1)", SOME 1)
                        val () =
                           S.readyAndSwitch
                           (fn () =>
                            (prio := 1
                             ; TransID.force rtxid
                             ; S.prepVal (rt, msg)))
                        val b = true
                        val () = debug' "Channel.sendPoll(3.1.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.sendPoll(3.1.2)"
                     in
                        b
                     end
                | NONE => 
                     let
                        val () = debug' "Channel.sendPoll(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.sendPoll(3.2.1)", SOME 1)
                        val b = false
                        val () = debug' "Channel.sendPoll(3.2.2)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.sendPoll(3.2.2)", SOME 1)
                        val () = S.atomicEnd ()
                        val () = debug' "Channel.sendPoll(3.2.3)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.sendPoll(3.2.2)"
                     in
                        b
                     end
            val () = debug' "Channel.sendPoll(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.sendPoll(4)"
         in
            b
         end

      fun recv (CHAN {prio, inQ, outQ}) =
         let
            val () = Assert.assertNonAtomic' "Channel.recv"
            val () = debug' "Channel.recv(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recv(1)"
            val () = S.atomicBegin ()
            val () = debug' "Channel.recv(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.recv(2)", SOME 1)
            val msg = 
               case cleanAndDeque outQ of
                  SOME (stxid, st) =>   
                     let
                        val () = debug' "Channel.recv(3.1.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recv(3.1.1)", SOME 1)
                        val msg =
                           S.switch
                           (fn rt =>
                            (prio := 1
                             ; TransID.force stxid
                             ; S.prepVal (st, rt)))
                        val () = debug' "Channel.recv(3.1.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.recv(3.1.1)"
                     in
                        msg
                     end
                | NONE =>
                     let
                        val () = debug' "Channel.recv(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recv(3.2.1)", SOME 1)
                        val msg =
                           S.atomicSwitchToNext
                           (fn rt => enqueAndClean (inQ, (TransID.mkTxId (), rt)))
                        val () = debug' "Channel.recv(3.2.2)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recv(3.2.2)", SOME 1)
                        val () = S.atomicEnd ()
                        val () = debug' "Channel.recv(3.2.3)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.recv(3.2.3)"
                     in
                        msg
                     end
            val () = debug' "Channel.recv(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recv(4)"
         in
            msg
         end

      fun recvEvt (CHAN {prio, inQ, outQ}) = 
         let
            fun doitFn () = 
               let
                  val () = Assert.assertAtomic' ("Channel.recvEvt.doitFn", NONE)
                  val (stxid, st) = valOf (Q.deque outQ)
                  val () = debug' "Channel.recvEvt(3.1.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.recvEvt(3.1.1)", SOME 1)
                  val msg =
                     S.switch
                     (fn rt =>
                      (prio := 1
                       ; TransID.force stxid
                       ; S.prepVal (st, rt)))
                  val () = debug' "Channel.recvEvt(3.1.2)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Channel.recvEvt(3.1.1)"
               in
                  msg
               end
            fun blockFn {transId, cleanUp, next} = 
               let
                  val () = Assert.assertAtomic' ("Channel.recvEvt.blockFn", NONE)
                  val () = debug' "Channel.recvEvt(3.2.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.recvEvt(3.2.1)", SOME 1)
                  val msg =
                     S.atomicSwitch
                     (fn rt =>
                      (enqueAndClean (inQ, (transId, rt))
                       ; next ()))
                  val () = debug' "Channel.recvEvt(3.2.2)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.recvEvt(3.2.2)", SOME 1)
                  val () = cleanUp ()
                  val () = S.atomicEnd ()
                  val () = debug' "Channel.recvEvt(3.2.3)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Channel.recvEvt(3.2.3)"
               in
                  msg
               end
            fun pollFn () = 
               let
                  val () = Assert.assertAtomic' ("Channel.recvEvt.pollFn", NONE)
                  val () = debug' "Channel.recvEvt(2)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.recvEvt(2)", SOME 1)
               in
                  case cleanAndChk (prio, outQ) of 
                     0 => E.blocked blockFn
                   | prio => E.enabled {prio = prio, doitFn = doitFn}
               end
         in
            E.bevt pollFn
         end

      fun recvPoll (CHAN {prio, outQ, ...}) = 
         let
            val () = Assert.assertNonAtomic' "Channel.recvPoll"
            val () = debug' "Channel.recvPoll(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recvPoll(1)"
            val () = S.atomicBegin ()
            val () = debug' "Channel.recvPoll(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.recvPoll(2)", SOME 1)
            val msg =
               case cleanAndDeque outQ of 
                  SOME (stxid, st) => 
                     let
                        val () = debug' "Channel.recvPoll(3.1.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recvPoll(3.1.1)", SOME 1)
                        val msg =
                           S.switch
                           (fn rt =>
                            (prio := 1
                             ; TransID.force stxid
                             ; S.prepVal (st, rt)))
                        val msg = SOME msg
                        val () = debug' "Channel.recvPoll(3.1.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.recvPoll(3.1.1)"
                     in
                        msg
                     end
                | NONE => 
                     let
                        val () = debug' "Channel.recv(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recv(3.2.1)", SOME 1)
                        val msg = NONE
                        val () = debug' "Channel.recvPoll(3.2.2)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recvPoll(3.2.2)", SOME 1)
                        val () = S.atomicEnd ()
                        val () = debug' "Channel.recvPoll(3.2.3)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.recvPoll(3.2.3)"
                     in
                        msg
                     end
            val () = debug' "Channel.recvPoll(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recvPoll(4)"
         in
            msg
         end
   end
