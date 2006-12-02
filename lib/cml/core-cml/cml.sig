(* cml.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* cml-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The interface to the core CML features.
 *)

signature CML =
  sig
     include VERSION

     include THREAD
     include CHANNEL
     include EVENT
     include TIME_OUT
  end
