(* cvar.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* ???
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature CVAR =
   sig
      datatype cvar = datatype RepTypes.cvar
      datatype cvar_state = datatype RepTypes.cvar_state

      val new : unit -> cvar
   end
