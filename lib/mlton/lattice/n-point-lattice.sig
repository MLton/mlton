(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature N_POINT_LATTICE_STRUCTS = 
   sig
      (* pretty print names *)
      val names: string list
   end

signature N_POINT_LATTICE = 
   sig
      include N_POINT_LATTICE_STRUCTS

      type t

      val <= : t * t -> unit (* force rhs to be up-ed if lhs is *)
      val == : t * t -> unit (* force lhs and rhs to be the same *)
      val isN: t * int -> bool
      val layout: t -> Layout.t
      val makeN: t * int -> unit
      val new: unit -> t (* a new 0 *)
      val up: t -> unit
      (* handler will be run once iff value becomes gte N *)
      val whenN: t * int * (unit -> unit) -> unit
   end
