(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WEIGHT = 
   sig
      include BOUNDED_ORDER
      val zero : t
      val + : t * t -> t
      val input : In.t -> t
      val output : t * Out.t -> unit
   end
