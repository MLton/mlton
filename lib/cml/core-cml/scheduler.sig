(* scheduler.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* scheduler.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature SCHEDULER =
   sig
      include CRITICAL

      type thread_id = ThreadID.thread_id
      type 'a thread = 'a RepTypes.thread
      type rdy_thread = RepTypes.rdy_thread

      val prep : unit thread -> rdy_thread
      val prepVal : 'a thread * 'a -> rdy_thread
      val prepFn : 'a thread * (unit -> 'a) -> rdy_thread

      val getThreadId : 'a thread -> thread_id
      val getCurThreadId : unit -> thread_id
      val tidMsg : unit -> string

      val ready : rdy_thread -> unit
      val next : unit -> rdy_thread

      val switch : ('a thread -> rdy_thread) -> 'a
      val atomicSwitch : ('a thread -> rdy_thread) -> 'a
      val switchToNext : ('a thread -> unit) -> 'a
      val atomicSwitchToNext : ('a thread -> unit) -> 'a
      val readyAndSwitch : (unit -> rdy_thread) -> unit
      val atomicReadyAndSwitch : (unit -> rdy_thread) -> unit
      val readyAndSwitchToNext : (unit -> unit) -> unit
      val atomicReadyAndSwitchToNext : (unit -> unit) -> unit

      val new : (thread_id -> ('a -> unit)) -> 'a thread

      val prepend : 'a thread * ('b -> 'a) -> 'b thread
      val unwrap : (rdy_thread -> rdy_thread) -> 
                   (MLton.Thread.Runnable.t -> MLton.Thread.Runnable.t)

      val reset : bool -> unit
      val preempt : rdy_thread -> unit
   end
