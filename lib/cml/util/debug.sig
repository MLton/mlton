(* debug.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* debug.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Debugging support for the CML core.
 *)

signature DEBUG =
   sig
      val sayDebug  : (unit -> string) list * (unit -> string) -> unit
      val sayDebug' : string -> unit
   end
