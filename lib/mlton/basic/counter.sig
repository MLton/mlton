(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature COUNTER =
   sig
      type t

      val equals: t * t -> bool
      val generator: int -> (unit -> int)
      val new: int -> t
      val next: t -> int
      val reset: t * int -> unit
      val tick: t -> unit
      val value: t -> int
   end
