(* scheduler-hooks.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* scheduler.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature SCHEDULER_HOOKS =
   sig
      type 'a thread = 'a RepTypes.thread
      type rdy_thread = RepTypes.rdy_thread

      (* this hook gets invoked when the scheduler has nothing else to do;
       * it is invoked in an atomic region
       *)
      val pauseHook : (unit -> rdy_thread) ref

      (* this hook points to a thread that gets invoked when
       * the system is otherwise deadlocked.  It takes two arguments:
       * the first is a boolean flag that says weather to do clean-up,
       * and the second is the exit status.
       *)
      val shutdownHook : (bool * OS.Process.status) thread ref

      val reset : unit -> unit
   end
