(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INTERMEDIATE_COMPUTATION_STRUCTS =
   sig
   end

signature INTERMEDIATE_COMPUTATION = 
   sig
      include INTERMEDIATE_COMPUTATION_STRUCTS

      structure Computation: COMPUTATION

      type t

      val empty: unit -> t
      val call: t * string * (unit -> Layout.t) -> unit
      val raisee: t * Time.t option -> unit
      val return: t * (unit -> Layout.t) * Time.t option -> unit
      val finish: t -> Computation.t
      val atTopLevel: t -> bool
   end
