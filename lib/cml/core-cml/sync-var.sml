(* sync-var.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* sync-var.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The implementation of Id-style synchronizing memory cells.
 *)

structure SyncVar : SYNC_VAR_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure Q = ImpQueue
      structure S = Scheduler
      structure E = Event
      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)

      datatype trans_id = datatype TransID.trans_id
      datatype trans_id_state = datatype TransID.trans_id_state


      (* the underlying representation of both ivars and mvars is the same. *)
      datatype 'a cell = 
         CELL of {prio  : int ref,
                  readQ : (trans_id * 'a S.thread) Q.t,
                  value : 'a option ref}

      type 'a ivar = 'a cell
      type 'a mvar = 'a cell

      exception Put

      fun newCell () = CELL {prio = ref 0, readQ = Q.new(), value = ref NONE}

      (* sameCell : ('a cell * 'a cell) -> bool *)
      fun sameCell (CELL {prio = prio1, ...}, CELL {prio = prio2, ...}) =
         prio1 = prio2

      (* bump a priority value by one, returning the old value *)
      fun bumpPriority (p as ref n) = (p := n+1; n)

      (* functions to clean channel input and output queues *)
      local
         fun cleaner (TXID txst, _) = 
            case !txst of CANCEL => true | _ => false
      in
         fun cleanAndDeque q =
            Q.cleanAndDeque (q, cleaner)
         fun enqueAndClean (q, item) =
            Q.enqueAndClean (q, item, cleaner)
      end

      (* When a thread is resumed after being blocked on an iGet or mGet operation,
       * there may be other threads also blocked on the variable.  This function
       * is used to propagate the message to all of the threads that are blocked
       * on the variable (or until one of them takes the value in the mvar case).
       * It must be called from an atomic region; when the readQ is finally empty,
       * we leave the atomic region.  We must use "cleanAndDeque" to get items
       * from the readQ in the unlikely event that a single thread executes a
       * choice of multiple gets on the same variable.
       *)
      fun relayMsg (readQ, msg) = 
         case (cleanAndDeque readQ) of
            NONE => S.atomicEnd()
          | SOME (txid, t) => 
               S.readyAndSwitch
               (fn () =>
                (TransID.force txid
                 ; S.prepVal (t, msg)))

      (** G-variables **)
      (* Generalized synchronized variables,
       * to factor out the common operations. 
       *)

      fun gPut (name, CELL {prio, readQ, value}, x) = 
         let
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name])
            val () = debug (fn () => concat [name, "(1)"]) (* NonAtomic *)
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(1)"])
            val () = S.atomicBegin()
            val () = debug (fn () => concat [name, "(2)"]) (* Atomic 1 *)
            val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
            val () = 
               case !value of
                  NONE => 
                     let
                        val () = debug (fn () => concat [name, "(3.1.1)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.1.1)"], SOME 1)
                        val () = value := SOME x
                        val () = 
                           case cleanAndDeque readQ of
                              NONE => S.atomicEnd ()
                            | SOME (rtxid, rt) =>
                                 S.readyAndSwitch
                                 (fn () =>
                                  (prio := 1
                                   ; TransID.force rtxid
                                   ; S.prepVal (rt, x)))
                        val () = debug (fn () => concat [name, "(3.1.2)"]) (* NonAtomic *)
                        val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.1.2)"])
                     in
                        ()
                     end
                | SOME _ => 
                     let
                        val () = debug (fn () => concat [name, "(3.2.1)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                        val () = S.atomicEnd ()
                        val () = debug (fn () => concat [name, "(3.2.2)"]) (* NonAtomic *)
                        val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
                     in 
                        raise Put
                     end
            val () = debug (fn () => concat [name, "(4)"]) (* NonAtomic *)
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(4)"])
         in
            ()
         end

      (* Swap the current contents of the cell with a new value;
       * it is guaranteed to be atomic.
       *)
      fun gSwap (name, doSwap, CELL {prio, readQ, value}) = 
         let
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, ""])
            val () = debug (fn () => concat [name, "(1)"]) (* NonAtomic *)
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(1)"])
            val () = S.atomicBegin()
            val () = debug (fn () => concat [name, "(2)"]) (* Atomic 1 *)
            val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
            val msg =
               case !value of
                  NONE => 
                     let
                        val () = debug (fn () => concat [name, "(3.2.1)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                        val msg = 
                           S.atomicSwitchToNext
                           (fn rt => enqueAndClean (readQ, (TransID.mkTxId (), rt)))
                        val () = debug (fn () => concat [name, "(3.2.2)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"], SOME 1)
                        val () = doSwap value
                        val () = relayMsg (readQ, msg)
                        val () = debug (fn () => concat [name, "(3.2.3)"]) (* NonAtomic *)
                        val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.3)"])
                     in
                        msg
                     end
                | SOME x => 
                     let
                        val () = debug (fn () => concat [name, "(3.2.1)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                        val () = prio := 1
                        val () = doSwap value
                        val () = S.atomicEnd ()
                        val () = debug (fn () => concat [name, "(3.2.2)"]) (* NonAtomic *)
                        val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
                     in
                        x
                     end
            val () = debug (fn () => concat [name, "(4)"]) (* NonAtomic *)
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(4)"])
         in
            msg
         end

      fun gSwapEvt (name, doSwap, CELL{prio, readQ, value}) = 
         let
            fun doitFn () =
               let
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, ".doitFn"], NONE)
                  val x = valOf (!value)
                  val () = debug (fn () => concat [name, "(3.2.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                  val () = prio := 1
                  val () = doSwap value
                  val () = S.atomicEnd ()
                  val () = debug (fn () => concat [name, "(3.2.2)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
               in
                  x
               end
            fun blockFn {transId, cleanUp, next} = 
               let
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, ".blockFn"], NONE)
                  val () = debug (fn () => concat [name, "(3.2.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                  val msg = 
                     S.atomicSwitch
                     (fn rt =>
                      (enqueAndClean (readQ, (transId, rt))
                       ; next ()))
                  val () = debug (fn () => concat [name, "(3.2.2)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"], SOME 1)
                  val () = cleanUp ()
                  val () = doSwap value
                  val () = relayMsg (readQ, msg)
                  val () = debug (fn () => concat [name, "(3.2.3)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.3)"])
               in
                  msg
               end
            fun pollFn () =
               let
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, ".pollFn"], NONE)
                  val () = debug (fn () => concat [name, "(2)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
               in
                  case !value of
                     NONE => E.blocked blockFn
                   | SOME _ => E.enabled {prio = bumpPriority prio,
                                          doitFn = doitFn}
               end
         in
            E.bevt pollFn
         end

      fun gSwapPoll (name, doSwap, CELL{prio, value, ...}) = 
         let
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, ""])
            val () = debug (fn () => concat [name, "(1)"]) (* NonAtomic *)
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(1)"])
            val () = S.atomicBegin()
            val () = debug (fn () => concat [name, "(2)"]) (* Atomic 1 *)
            val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
            val msg =
               case !value of
                  NONE => 
                     let
                        val () = debug (fn () => concat [name, "(3.2.1)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                        val msg = NONE
                        val () = debug (fn () => concat [name, "(3.2.2)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"], SOME 1)
                        val () = S.atomicEnd ()
                        val () = debug (fn () => concat [name, "(3.2.3)"]) (* NonAtomic *)
                        val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.3)"])
                     in
                        msg
                     end
                | SOME x => 
                     let
                        val () = debug (fn () => concat [name, "(3.2.1)"]) (* Atomic 1 *)
                        val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                        val () = prio := 1
                        val () = doSwap value
                        val () = S.atomicEnd ()
                        val () = debug (fn () => concat [name, "(3.2.2)"]) (* NonAtomic *)
                        val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
                     in
                        SOME x
                     end
            val () = debug (fn () => concat [name, "(4)"]) (* NonAtomic *)
            val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(4)"])
         in
            msg
         end


      (** I-variables **)

      val iVar = newCell
      val sameIVar = sameCell

      fun iPut (cell, x) = gPut ("iPut", cell, x)
      local fun doGetSwap _ = ()
      in
         fun iGet cell = gSwap ("iGet", doGetSwap, cell)
         fun iGetEvt cell = gSwapEvt ("iGetEvt", doGetSwap, cell)
         fun iGetPoll cell = gSwapPoll ("iGetPoll", doGetSwap, cell)
      end

      (** M-variables **)

      val mVar = newCell
      fun mVarInit x = CELL {prio = ref 0, readQ = Q.new(), value = ref (SOME x)}
      val sameMVar = sameCell

      fun mPut (cell, x) = gPut ("mPut", cell, x)
      local fun doTakeSwap value = value := NONE
      in
         fun mTake cell = gSwap ("mTake", doTakeSwap, cell)
         fun mTakeEvt cell = gSwapEvt ("mTakeEvt", doTakeSwap, cell)
         fun mTakePoll cell = gSwapPoll ("mTakePoll", doTakeSwap, cell)
      end
      local fun doGetSwap _ = ()
      in
         fun mGet cell = gSwap ("mGet", doGetSwap, cell)
         fun mGetEvt cell = gSwapEvt ("mGetEvt", doGetSwap, cell)
         fun mGetPoll cell = gSwapPoll ("mGetPoll", doGetSwap, cell)
      end
      local fun doSwapSwap x value = value := SOME x
      in
         fun mSwap (cell, x) = gSwap ("mSwap", doSwapSwap x, cell)
         fun mSwapEvt (cell, x) = gSwapEvt ("mSwap", doSwapSwap x, cell)
      end
   end
