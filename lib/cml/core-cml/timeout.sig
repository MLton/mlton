(* timeout.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* timeout-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Exported interface for timeout synchronization.
 *)

signature TIME_OUT =
   sig
      val timeOutEvt : Time.time -> unit Event.event
      val atTimeEvt  : Time.time -> unit Event.event
   end

signature TIME_OUT_EXTRA =
   sig
      include TIME_OUT

      val reset : unit -> unit
      (* preepmt () == NONE  ==>  no waiting threads 
       * preepmt () == SOME NONE  ==>  enqueued a waiting thread
       * preepmt () == SOME (SOME t)  ==>  a waiting thread will be ready in t time
       *)
      val preempt : unit -> Time.time option option
   end
