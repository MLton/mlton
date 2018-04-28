(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FLAT_LATTICE_STRUCTS =
   sig
      structure Point:
         sig
            type t

            val equals: t * t -> bool
            val layout: t -> Layout.t
         end
   end

signature FLAT_LATTICE =
   sig
      include FLAT_LATTICE_STRUCTS

      type t

      val <= : t * t -> bool
      val forcePoint: t * Point.t -> bool
      val forceTop: t -> bool
      val getPoint: t -> Point.t option
      val isBottom: t -> bool
      val isPoint: t -> bool
      val isPointEq: t * Point.t -> bool
      val isTop: t -> bool
      val layout: t -> Layout.t
      val lowerBound: t * Point.t -> bool
      val new: unit -> t
      val point: Point.t -> t
      val upperBound: t * Point.t -> bool
   end
