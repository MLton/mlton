(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature THREE_POINT_LATTICE_STRUCTS = 
   sig
      (* pretty print names *)
      val bottom: string
      val mid: string
      val top: string
   end

signature THREE_POINT_LATTICE = 
   sig
      include THREE_POINT_LATTICE_STRUCTS

      type t

      val <= : t * t -> unit (* force rhs to be mid/top if lhs is *)
      val == : t * t -> unit (* force lhs and rhs to be the same *)
      val isBottom: t -> bool
      val isMid: t -> bool
      val isTop: t -> bool
      val layout: t -> Layout.t
      val makeTop: t -> unit
      val makeMid: t -> unit
      val new: unit -> t (* a new bottom *)
      val up: t -> unit
      (* handler will be run once iff value gte mid *)
      val whenMid: t * (unit -> unit) -> unit
      (* handler will be run once iff value gte top *)
      val whenTop: t * (unit -> unit) -> unit
   end
