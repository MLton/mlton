(* event.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* events-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of event values and the event combinators.
 *)

signature EVENT =
   sig
      type 'a event

      val never     : 'a event
      val alwaysEvt : 'a -> 'a event

      val wrap        : ('a event * ('a -> 'b)) -> 'b event
      val wrapHandler : ('a event * (exn -> 'a)) -> 'a event

      val guard    : (unit -> 'a event) -> 'a event
      val withNack : (unit event -> 'a event) -> 'a event

      val choose : 'a event list -> 'a event
      val sync : 'a event -> 'a
      val select : 'a event list -> 'a
   end

signature EVENT_EXTRA =
   sig
      include EVENT

      type 'a status
      val enabled : {prio : int, doitFn : unit -> 'a} -> 'a status
      val blocked : ({transId : TransID.trans_id,
                      cleanUp : unit -> unit,
                      next : unit -> Scheduler.rdy_thread} -> 'a) -> 'a status
      val bevt : (unit -> 'a status) -> 'a event

      val atomicCVarSet : CVar.cvar -> unit
      val cvarGetEvt    : CVar.cvar -> unit event
   end
