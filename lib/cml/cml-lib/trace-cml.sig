(* trace-cml-sig.sml
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

signature TRACE_CML =
  sig

  (** Trace modules **
   *
   * The basic idea is that one defines a heirarchy of ``trace
   * modules,'' which provide valves for debugging output.
   *)

    type trace_module

  (* where to direct trace output to *)
    datatype trace_to
      = TraceToOut
      | TraceToErr
      | TraceToNull
      | TraceToFile of string
      | TraceToStream of TextIO.outstream

    val setTraceFile : trace_to -> unit
        (* Direct the destination of trace output.  Note: TraceToStream
         * can only be specified as a destination if CML is running.
         *)

    val traceRoot : trace_module
        (* the root module of the trace hierarchy *)

    exception NoSuchModule

    val traceModule : (trace_module * string) -> trace_module
    val nameOf : trace_module -> string
        (* return the name of the module *)
    val moduleOf : string -> trace_module
        (* return the module specified by the given string, or raise
         * NoSuchModule if none exists.
         *)

    val traceOn : trace_module -> unit
        (* turn tracing on for a module and its descendents *)
    val traceOff : trace_module -> unit
        (* turn tracing off for a module and its descendents *)
    val traceOnly : trace_module -> unit
        (* turn tracing on for a module (but not for its descendents) *)
    val amTracing : trace_module -> bool
        (* return true if this module is being traced *)

    val status : trace_module -> (trace_module * bool) list
        (* return a list of the registered modules dominated by the given
         * module, and their status.
         *)

    val trace : (trace_module * (unit -> string list)) -> unit
        (* conditionally generate tracing output *)


  (** Thread watching **)

    val watcher : trace_module
        (* controls printing of thread watching messages; the module's name
         * is "/ThreadWatcher/"
         *)
    val watch : (string * CML.thread_id) -> unit
        (* watch the given thread for unexpected termination *)
    val unwatch : CML.thread_id -> unit
        (* stop watching the named thread *)

  (** Uncaught exception handling **)

    val setUncaughtFn : ((CML.thread_id * exn) -> unit) -> unit
        (* this sets the default uncaught exception action. *)
    val setHandleFn : ((CML.thread_id * exn) -> bool) -> unit
        (* add an additional uncaught exception action.  If the action returns
         * true, then no further action is taken.  This can be used to handle
         * application specific exceptions.
         *)
    val resetUncaughtFn : unit -> unit
        (* this resets the default uncaught exception action to the system default,
         * and removes any layered actions.
         *)

  end; (* TRACE_CML *)

