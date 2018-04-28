(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RING_STRUCTS =
   sig
      type t

      val + : t * t -> t
      val ~ : t -> t
      val * : t * t -> t
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val zero: t
   end

signature RING =
   sig
      include RING_STRUCTS

      val - : t * t -> t
      val double: t -> t
      val isZero: t -> bool
      val square: t -> t
      val sum: t list -> t
   end
