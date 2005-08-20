(* trace-cml.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * This module provides rudimentary debugging support in the form of mechanisms
 * to control debugging output, and to monitor thread termination.  This
 * version of this module is adapted from Cliff Krumvieda's utility for tracing
 * CML programs.  It provides three facilities: trace modules, for controlling
 * debugging output; thread watching, for detecting thread termination; and
 * a mechanism for reporting uncaught exceptions on a per thread basis.
 *)

structure TraceCML : TRACE_CML =
  struct

    structure SV = SyncVar

  (* where to direct trace output to *)
    datatype trace_to
      = TraceToOut
      | TraceToErr
      | TraceToNull
      | TraceToFile of string
      | TraceToStream of TextIO.outstream

    exception NoSuchModule

  (** Trace Modules **)
    datatype trace_module = TM of {
        full_name : string,
        label : string,
        tracing : bool ref,
        children : trace_module list ref
      }

    val traceRoot = TM{
            full_name = "/",
            label = "",
            tracing = ref false,
            children = ref []
          }

    fun forAll f = let
          fun for (tm as TM{children, ...}) = (f tm; forChildren(!children))
          and forChildren [] = ()
            | forChildren (tm::r) = (for tm; forChildren r)
          in
            for
          end

    structure SS = Substring

    fun findTraceModule name = let
          fun eq ss (TM{label, ...}) = (SS.compare(SS.all label, ss) = EQUAL)
          fun find ([], tm) = SOME tm
            | find (arc::rest, tm as TM{label, children, ...}) = let
                val eqArc = eq arc
                fun findChild [] = NONE
                  | findChild (c::r) =
                      if (eqArc c) then find(rest, c) else findChild r
                in
                  findChild (!children)
                end
          in
            find (
              SS.tokens (fn #"/" => true | _ => false) (SS.all name),
              traceRoot)
          end

    fun traceModule' (TM parent, name) = let
          fun checkChildren [] = let
                val tm = TM{
                        full_name = (#full_name parent ^ name),
                        label = name,
                        tracing = ref(!(#tracing parent)),
                        children = ref []
                      }
                in
                  (#children parent) := tm :: !(#children parent);
                  tm
                end
            | checkChildren((tm as TM{label, ...})::r) =
                if (label = name) then tm else checkChildren r
          in
            checkChildren (! (#children parent))
          end

  (* return the name of the module *)
    fun nameOf (TM{full_name, ...}) = full_name

  (* return the module specified by the given string *)
    fun moduleOf' name = (case findTraceModule name
           of NONE => raise NoSuchModule
            | (SOME tm) => tm
          (* end case *))

  (* turn tracing on for a module and its descendents *)
    val traceOn' = forAll (fn (TM{tracing, ...}) => tracing := true)

  (* turn tracing off for a module and its descendents *)
    val traceOff' = forAll (fn (TM{tracing, ...}) => tracing := false)

  (* turn tracing on for a module (but not for its descendents) *)
    fun traceOnly' (TM{tracing, ...}) = tracing := true

  (* return true if this module is being traced *)
    fun amTracing (TM{tracing, ...}) = !tracing

  (* return a list of the registered modules dominated by the given
   * module, and their status.
   *)
    fun status' root = let
          fun list (tm as TM{tracing, children, ...}, l) =
                listChildren (!children, (tm, !tracing)::l)
          and listChildren ([], l) = l
            | listChildren (c::r, l) = listChildren(r, list(c, l))
          in
            rev (list (root, []))
          end

  (** Trace printing **)
    val traceDst = ref TraceToOut
    val traceCleanup = ref (fn () => ())

    fun setTraceFile'  t = traceDst := t

(** NOTE: there are bookkeeping bugs, when changing the trace destination
 ** from TraceToStream to something else (where the original destination 
 ** was TraceToFile).
 **)
    fun tracePrint s = let
          fun output strm = (TextIO.output(strm, s); TextIO.flushOut strm)
          in
            case !traceDst
             of TraceToOut => output TextIO.stdOut
              | TraceToErr => output TextIO.stdErr
              | TraceToNull => ()
              | (TraceToFile fname) => let
                  val dst = let
                        val strm = TextIO.openOut fname
                        in
                          traceCleanup := (fn () => TextIO.closeOut strm);
                          TraceToStream strm
                        end handle _ => (
                          Debug.sayDebug(concat[
                              "TraceCML: unable to open \"", fname,
                              "\", redirecting to stdout"
                            ]);
                          TraceToOut)
                  in
                    setTraceFile' dst;
                    tracePrint s
                  end
                | (TraceToStream strm) => output strm
            (* end case *)
          end

  (** Trace server **)
    val traceCh : (unit -> string list) CML.chan = CML.channel()
    val traceUpdateCh : (unit -> unit) CML.chan = CML.channel()

    fun traceServer () = let
          val evt = [
                  CML.wrap(CML.recvEvt traceCh, fn f => tracePrint(concat(f()))),
                  CML.wrap(CML.recvEvt traceUpdateCh, fn f => f())
                ]
          fun loop () = (CML.select evt; loop())
          in
            loop()
          end (* traceServer *)

    fun tracerStart () = (CML.spawn traceServer; ())
    fun tracerStop () = ((!traceCleanup)(); traceCleanup := (fn () => ()))

    val _ = (
          RunCML.logChannel ("TraceCML:trace", traceCh);
          RunCML.logChannel ("TraceCML:trace-update", traceUpdateCh);
          RunCML.logServer ("TraceCML:trace-server", tracerStart, tracerStop))

    local
      fun carefully f = if RunCML.isRunning()
            then CML.send(traceUpdateCh, f)
            else f()
      fun carefully' f = if RunCML.isRunning()
              then let
                val reply = SV.iVar()
                in
                  CML.send (traceUpdateCh, fn () => (SV.iPut(reply, f())));
                  SV.iGet reply
                end
              else f()
    in
    fun traceModule arg = carefully' (fn () => traceModule' arg)
    fun moduleOf name = carefully' (fn () => moduleOf' name)
    fun traceOn tm = carefully (fn () => traceOn' tm)
    fun traceOff tm = carefully (fn () => traceOff' tm)
    fun traceOnly tm = carefully (fn () => traceOnly' tm)
    fun setTraceFile f = carefully (fn () => setTraceFile' f)
    fun status root = carefully' (fn () => status' root)
    end (* local *)

    fun trace (TM{tracing, ...}, prFn) =
          if (RunCML.isRunning() andalso (!tracing))
            then CML.send(traceCh, prFn)
            else ()


  (** Thread watching **)

  (* controls printing of thread watching messages *)
    val watcher = traceModule (traceRoot, "ThreadWatcher")
    val _ = traceOn watcher

    datatype watcher_msg
      = WATCH of (CML.thread_id * unit CML.chan)
      | UNWATCH of (CML.thread_id * unit SV.ivar)

    val watcherMb : watcher_msg Mailbox.mbox = Mailbox.mailbox ()

  (* stop watching the named thread *)
    fun unwatch tid = let
          val ackV = SV.iVar()
          in
            Mailbox.send(watcherMb, UNWATCH(tid, ackV));
            SV.iGet ackV
          end

  (* watch the given thread for unexpected termination *)
    fun watch (name, tid) = let
          val unwatchCh = CML.channel()
          fun handleTermination () = (
                trace (watcher, fn () => [
                    "WARNING!  Watched thread ", name, CML.tidToString tid,
                    " has died.\n"
                  ]);
                unwatch tid)
          fun watcherThread () = (
                Mailbox.send (watcherMb, WATCH(tid, unwatchCh));
                CML.select [
                    CML.recvEvt unwatchCh,
                    CML.wrap (CML.joinEvt tid, handleTermination)
                  ])
          in
            CML.spawn (watcherThread); ()
          end

    structure TidTbl = HashTableFn (
      struct
        type hash_key = CML.thread_id
        val hashVal = CML.hashTid
        val sameKey = CML.sameTid
      end)

  (* the watcher server *)
    fun startWatcher () = let
          val tbl = TidTbl.mkTable (32, Fail "startWatcher")
          fun loop () = (case (Mailbox.recv watcherMb)
                 of (WATCH arg) => TidTbl.insert tbl arg
                  | (UNWATCH(tid, ack)) => (
                    (* notify the watcher that the thread is no longer being
                     * watched, and then acknowledge the unwatch command.
                     *)
                      CML.send(TidTbl.remove tbl tid, ())
                        handle _ => ();
                    (* acknowledge that the thread has been removed *)
                      SV.iPut(ack, ()))
                (* end case *);
                loop ())
          in
            CML.spawn loop; ()
          end

    val _ = (
          RunCML.logMailbox ("TraceCML:watcherMb", watcherMb);
          RunCML.logServer ("TraceCML:watcher-server", startWatcher, fn () => ()))


  (** Uncaught exception handling **)

    fun defaultHandlerFn (tid, ex) = let
          val raisedAt = (case (SMLofNJ.exnHistory ex)
                 of [] => ["\n"]
                  | l => [" raised at ", List.last l, "\n"]
                (* end case *))
          in
            Debug.sayDebug (concat ([
                CML.tidToString tid, " uncaught exception ",
                exnName ex, " [", exnMessage ex, "]"
              ] @ raisedAt))
          end

    val defaultHandler = ref defaultHandlerFn
    val handlers = ref ([] : ((CML.thread_id * exn) -> bool) list)

  (* this sets the default uncaught exception action. *)
    fun setUncaughtFn' action = defaultHandler := action

  (* add an additional uncaught exception action.  If the action returns
   * true, then no further action is taken.  This can be used to handle
   * handle application specific exceptions.
   *)
    fun setHandleFn' action = handlers := action :: !handlers

  (* this resets the default uncaught exception action to the system default,
   * and removes any layered actions.
   *)
    fun resetUncaughtFn' () = (defaultHandler := defaultHandlerFn; handlers := [])

    val exnUpdateCh : (unit -> unit) CML.chan = CML.channel()

    fun exnServerStartup () = let
          val errCh = Mailbox.mailbox()
        (* this function is installed as the default handler for threads;
         * it sends the thread ID and uncaught exception to the ExnServer.
         *)
          fun threadHandler exn = Mailbox.send(errCh, (CML.getTid(), exn))
        (* invoke the hsndler actions on the uncaught exception *)
          fun handleExn arg = let
                val hdlrList = !handlers and dfltHndlr = !defaultHandler
                fun loop [] = dfltHndlr arg
                  | loop (hdlr::r) = if (hdlr arg) then () else loop r
                in
                  CML.spawn (fn () => ((loop hdlrList) handle _ => (dfltHndlr arg)));
                  ()
                end
          val event = [
                  CML.wrap (CML.recvEvt exnUpdateCh, fn f => f()),
                  CML.wrap (Mailbox.recvEvt errCh, handleExn)
                ]
          fun server () = (CML.select event; server())
          in
            Thread.defaultExnHandler := threadHandler;
            CML.spawn server; ()
          end

    val _ = (
          RunCML.logChannel ("TraceCML:exnUpdateCh", exnUpdateCh);
          RunCML.logServer ("TraceCML", exnServerStartup, fn () => ()))

    local
      fun carefully f = if RunCML.isRunning() then CML.send(exnUpdateCh, f) else f()
    in
    fun setUncaughtFn arg = carefully (fn () => setUncaughtFn' arg)
    fun setHandleFn arg = carefully (fn () => setHandleFn' arg)
    fun resetUncaughtFn arg = carefully (fn () => resetUncaughtFn' arg)
    end (* local *)

  end; (* TraceCML *)
