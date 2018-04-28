(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature UNIQUE_ID =
   sig
      type t

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val new: unit -> t
      val toString: t -> string
   end
