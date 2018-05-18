(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature UNIVERSE =
   sig
      include SET

      structure B: T

      datatype elt =
         Base of B.t
       | Pair of elt * elt
       | Set of t
      sharing type elt = E.t

      val toBase: elt -> B.t
      val toPair: elt -> elt * elt
      val toSet: elt -> t

      val cross: t * t -> t
      val project1: t -> t
      val project2: t -> t

      val Union: t -> t
   (*   val Cross: t -> t *)

      val lookup: t * E.t -> E.t option
      val update: t * E.t * E.t -> t
      val updateSet: t * t -> t
    end
