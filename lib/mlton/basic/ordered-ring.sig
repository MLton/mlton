(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ORDERED_RING_STRUCTS =
   sig
      include RING_WITH_IDENTITY

      val compare: t * t -> Relation.t
      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
   end

signature ORDERED_RING =
   sig
      include ORDERED_RING_STRUCTS

      val abs: t -> t
      val factorial: t -> t
      val foldl: t * t * 'a * ('a * t -> 'a) -> 'a
      val isNegative: t -> bool
      val isPositive: t -> bool
      val max: t * t -> t
      val min: t * t -> t
      val prodFromTo: {from: t, to: t, term: t -> t} -> t
      val sumFromTo: {from: t, to: t, term: t -> t} -> t 
   end
