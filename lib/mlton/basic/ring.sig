(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
