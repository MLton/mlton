(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BOOL =
   sig
      type t

      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val fromString: string -> t option
      val layout: t -> Layout.t
      val not: t -> t
      val toString: t -> string
   end where type t = bool
