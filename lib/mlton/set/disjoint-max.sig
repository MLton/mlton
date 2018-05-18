(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DISJOINT_MAX =
   sig
      structure O: ORDER

      type t
      val singleton: O.t -> t
      val eval: t -> O.t 

      val link: t * t -> unit  (* must link roots *)
      (* Make second tree a child of first tree *)

      val update: t * O.t -> unit (* must update a root *)
   end
