(* cvar.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* ???
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure CVar : CVAR =
   struct
      structure R = RepTypes


      (* Condition variables are essentially unit valued ivars, and 
       * are used for various internal synchronization conditions 
       * (e.g., nack events, I/O synchronization, and thread termination).
       *)
      datatype cvar = datatype R.cvar
      datatype cvar_state = datatype R.cvar_state

      fun new () = CVAR (ref (CVAR_unset []))
   end
