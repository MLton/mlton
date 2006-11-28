(* run-cml.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* ???
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature RUN_CML =
   sig
      val isRunning : unit -> bool
      val doit : (unit -> unit) * Time.time option -> OS.Process.status
      val shutdown : OS.Process.status -> 'a
   end
