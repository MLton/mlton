(* result.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* result.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 *)

signature RESULT = 
   sig
      type 'a result

      val result : unit -> 'a result
      val put    : ('a result * 'a) -> unit
      val putExn : ('a result * exn) -> unit
      val get    : 'a result -> 'a
      val getEvt : 'a result -> 'a CML.event
   end
