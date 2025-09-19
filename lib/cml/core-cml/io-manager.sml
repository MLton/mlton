structure IOManager:
sig

  type iodesc
  type poll_desc
  type poll_info

  val ioEvt: poll_desc -> poll_info Event.event

  val pollIO: unit -> unit

  val anyWaiting: unit -> bool

end =
struct
  structure E = Event
  structure S = Scheduler

  type iodesc = OS.IO.iodesc
  type poll_desc = OS.IO.poll_desc
  type poll_info = OS.IO.poll_info

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
          val pi = S.atomicSwitch (fn thrd =>
            let
              val item =
                {pd = pd, tid = transId, cleanUp = cleanUp, thrd = thrd}
            in
              waiting := item :: !waiting;
              next ()
            end)
        in
          pi
        end
      fun pollFn () =
        case poll [pd] of
          [pi] => E.enabled {prio = ~1, doitFn = fn () => (S.atomicEnd (); pi)}
        | _ => E.blocked blockFn
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
        ( {tid as TXID (ref TRANS), cleanUp, thrd, ...}: io_wait_item
        , pi: poll_info
        ) =
        (TransID.force tid; cleanUp (); S.ready (S.prepVal (thrd, pi)))
    | enqueue ({tid = TXID (ref CANCEL), ...}, _) = ()

  fun pollIO () =
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
                       (enqueue (item, pi); filter (pis, r, wq))
                     else
                       filter (pi :: pis, r, item :: wq)
                 | filter _ = raise Fail "pollIO: unreachable case"
             in
               filter (l, wq, [])
             end)

  fun anyWaiting () =
    case !waiting of
      [] => false
    | _ => true
end
