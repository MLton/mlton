(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PAIR =
   sig
      structure X: T
      structure Y: T

      type t = X.t * Y.t
      val equals: t * t -> bool
      val layout: t -> Layout.t
   end
