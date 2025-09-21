structure IOManager:
sig

  type iodesc
  type poll_desc
  type poll_info
  datatype preempt_type = READIED | INQUEUE of poll_desc list | EMPTYQUEUE

  val ioEvt: poll_desc -> poll_info Event.event

  val preempt: unit -> preempt_type

  val anyWaiting: unit -> bool

end =
struct
  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  structure E = Event
  structure S = Scheduler

  type iodesc = OS.IO.iodesc
  type poll_desc = OS.IO.poll_desc
  type poll_info = OS.IO.poll_info

  fun debug msg =
    Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
  fun debug' msg =
    debug (fn () => msg)

  datatype preempt_type = READIED | INQUEUE of poll_desc list | EMPTYQUEUE
  datatype trans_id = datatype TransID.trans_id
  datatype trans_id_state = datatype TransID.trans_id_state

  type io_wait_item =
    { pd: poll_desc
    , tid: trans_id
    , cleanUp: unit -> unit
    , thrd: poll_info S.thread
    }

  val waiting = ref ([] : io_wait_item list)

  fun poll l =
    OS.IO.poll (l, SOME Time.zeroTime)
    handle _ => []

  fun ioEvt pd =
    let
      fun blockFn {transId, cleanUp, next} =
        let
          val () = Assert.assertAtomic' ("IOManager.ioEvt.blockFn", NONE)
          val () = debug' "ioEvt(1.1)" (* Atomic 1 *)
          val () = Assert.assertAtomic' ("IOManager.ioEvt(1.1)", SOME 1)
          val pi = S.atomicSwitch (fn thrd =>
            let
              val item =
                {pd = pd, tid = transId, cleanUp = cleanUp, thrd = thrd}
            in
              waiting := item :: !waiting;
              next ()
            end)
          val () = debug' "ioEvt(1.2)" (* NonAtomic *)
          val () = Assert.assertNonAtomic' "IOManager.ioEvt(1.2)"
        in
          pi
        end
      fun pollFn () =
        let
          val () = Assert.assertAtomic' ("IOManager.ioEvt.pollFn", NONE)
          val () = debug' "IOManager.ioEvt(2)" (* Atomic 1 *)
          val () = Assert.assertAtomic' ("IOManager.ioEvt(2)", SOME 1)
        in
          case poll [pd] of
            [pi] =>
              E.enabled {prio = ~1, doitFn = fn () => (S.atomicEnd (); pi)}
          | _ => E.blocked blockFn
        end
    in
      E.bevt pollFn
    end

  fun sameDesc (pi, pd) = OS.IO.infoToPollDesc pi = pd

  fun clean wq =
    let
      fun cl ([]: io_wait_item list, pds, q) = (pds, q)
        | cl ({tid = TXID (ref CANCEL), ...} :: r, pds, wq) = cl (r, pds, wq)
        | cl ((item as {pd, ...}) :: r, pds, wq) =
            cl (r, pd :: pds, item :: wq)
    in
      cl (wq, [], [])
    end

  fun enqueue
        ( readied
        , {tid as TXID (ref TRANS), cleanUp, thrd, ...}: io_wait_item
        , pi: poll_info
        ) =
        ( readied := true
        ; TransID.force tid
        ; cleanUp ()
        ; S.ready (S.prepVal (thrd, pi))
        )
    | enqueue (_, {tid = TXID (ref CANCEL), ...}, _) = ()

  fun preempt () : preempt_type =
    let
      val () = Assert.assertAtomic' ("IOManager.preempt", NONE)
      val () = debug' "IOManager.preempt" (* Atomic 1 *)
      val () = Assert.assertAtomic' ("IOManager.preempt", SOME 1)
      val readied = ref false
    in
      case clean (!waiting) of
        ([], _) => waiting := []
      | (pds, wq) =>
          (case poll pds of
             [] => waiting := List.rev wq
           | l =>
               let
                 fun filter ([], r, wq) =
                       waiting := List.revAppend (r, wq)
                   | filter (pi :: pis, (item: io_wait_item) :: r, wq) =
                       if sameDesc (pi, #pd item) then
                         (enqueue (readied, item, pi); filter (pis, r, wq))
                       else
                         filter (pi :: pis, r, item :: wq)
                   | filter _ = raise Fail "preempt: unreachable case"
               in
                 filter (l, wq, [])
               end);
      if !readied then
        READIED
      else
        case !waiting of
          [] => EMPTYQUEUE
        | items => INQUEUE (List.map #pd items)
    end

  fun anyWaiting () =
    case !waiting of
      [] => false
    | _ => true
end
