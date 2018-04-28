(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TRACE_CONTROL = 
   sig
      (* controls what tracing info is gathered *)
      val always: unit -> unit      (* always gather info *)
      val flagged: unit -> unit     (* only on flagged functions *)
      val never: unit -> unit       (* never gather info *)

      (* value of newly created flag *)
      val default: bool ref

      (* turn on/off tracing for given functions *)
      val on: string list -> unit
      val off: string list -> unit

      (* turn on/off tracing for all functions *)
      val all: unit -> unit
      val none: unit -> unit

      (* returns functions that are on/off *)
      val whatsOn: unit -> string list
      val whatsOff: unit -> string list
   end

signature TRACE =
   sig
      structure Immediate:
         sig
            include TRACE_CONTROL

            datatype debug =
               None
             | Terminal
             | Out of Out.t

            (*
             * If !debug = Terminal, debugging messages are printed to /dev/tty.
             * If !debug = Out os, then messages will be sent to os.
             * If !debug = None, then messages will be ignored.
             *)
            val debug: debug ref
            val message: Layout.t -> unit
            val messageStr: string -> unit
            (* inChildProcess is called by Process so that trace will know if it
             * is in a child, and therefore it will prefix all messages with the
             * pid of the current process.
             *)
            val inChildProcess: unit -> unit
            (* !showTime = true iff messages are preceded by the current time *)
            val showTime: bool ref
         end
      structure Delayed:
         sig
            include TRACE_CONTROL
            val keepAll: bool ref
         end
      structure Time: TRACE_CONTROL

      val never: unit -> unit
      val always: unit -> unit
      val flagged: unit -> unit
      val traceable: unit -> string list
      val outputTraceable: unit -> unit
      val reset: unit -> unit

      (*---------- Delayed Feedback ----------*)
      structure Computation: COMPUTATION

      (* clear computation history *)
      val clear: unit -> unit

      (* get computation history *)
      val computation: unit -> Computation.t

      (* show computation history *)
      val history: unit -> unit

      (* show computation history, without arguments or results *)
      val calls: unit -> unit

      val times: unit -> unit

      (* enter the inspector *)
      val inspect: unit -> unit

      (*---------- Instrumentation ----------*)

      val traceCall: string -> ('a -> 'b) -> 'a -> 'b

      type info
      val info: string -> info

      val traceInfo:
         info
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         * ('a -> bool * ('b -> bool))
         -> ('a -> 'b)
         -> ('a -> 'b)

      val traceInfo':
         info
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         -> ('a -> 'b)
         -> ('a -> 'b)

(*       type ('a, 'b) check = ('a -> Layout.t) * ('a -> bool * 'b)
 * 
 *       type ('a, 'b) z =
 *       'a -> ((unit -> Layout.t)
 *              * (unit -> bool)
 *              * 'a
 *              * 'b)
 *       
 *       val traceInfo:
 *       info
 *       * ('a, ('b, unit) check) check
 *       -> ('a -> 'b)
 *       -> 'a
 *       -> 'b
 *)

      val assertTrue: 'a -> (bool * ('b -> bool))

      val traceAssert:
         string
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         * ('a -> bool * ('b -> bool))
         -> ('a -> 'b)
         -> ('a -> 'b)

      val trace:
         string
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         -> ('a -> 'b)
         -> ('a -> 'b)

      val trace0:
         string
         * ('a -> Layout.t)
         -> (unit -> 'a)
         -> (unit -> 'a)

      val trace2:
         string
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         * ('c -> Layout.t)
         -> ('a * 'b -> 'c)
         -> ('a * 'b -> 'c)

      val trace3:
         string
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         * ('c -> Layout.t)
         * ('d -> Layout.t)
         -> ('a * 'b * 'c -> 'd)
         -> ('a * 'b * 'c -> 'd)

      val trace4:
         string
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         * ('c -> Layout.t)
         * ('d -> Layout.t)
         * ('e -> Layout.t)
         -> ('a * 'b * 'c * 'd -> 'e)
         -> ('a * 'b * 'c * 'd -> 'e)

      val trace5:
         string
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         * ('c -> Layout.t)
         * ('d -> Layout.t)
         * ('e -> Layout.t)
         * ('f -> Layout.t)
         -> ('a * 'b * 'c * 'd * 'e -> 'f)
         -> ('a * 'b * 'c * 'd * 'e -> 'f)

      val traceRec:
         string
         * ('a -> Layout.t)
         * ('b -> Layout.t)
         -> (('a -> 'b) -> ('a -> 'b))
         -> 'a -> 'b 
   end
