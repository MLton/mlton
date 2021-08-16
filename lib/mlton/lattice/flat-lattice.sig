(* Copyright (C) 2021 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FLAT_LATTICE_REC_STRUCTS =
   sig
      structure Point:
         sig
            type 'a t

            val clone: {clone: 'a -> 'a,
                        equals: 'a * 'a -> bool} -> 'a t -> 'a t
            val coerce: {clone: 'a -> 'a,
                         coerce: {from: 'a, to: 'a} -> unit,
                         equals: 'a * 'a -> bool} -> {from: 'a t, to: 'a t} -> bool
            val equals: ('a * 'a -> bool) -> 'a t * 'a t -> bool
            val layout: ('a -> Layout.t) -> 'a t -> Layout.t
            val unify: {equals: 'a * 'a -> bool,
                        unify: 'a * 'a -> unit} -> 'a t * 'a t -> bool
         end
      (* pretty print names *)
      val bottom: string
      val top: string
   end

signature FLAT_LATTICE_REC =
   sig
      include FLAT_LATTICE_REC_STRUCTS

      structure Value:
         sig
            datatype 'a t =
               Bottom
             | Point of 'a Point.t
             | Top

            val isBottom: 'a t -> bool
            val isPoint: 'a t -> bool
            val isPointEq: ('a * 'a -> bool) -> 'a t * 'a Point.t -> bool
            val isTop: 'a t -> bool

            val layout: ('a -> Layout.t) -> 'a t -> Layout.t
         end

      type 'a t

      (* handler will be run once for each value *)
      val addHandler': 'a t * ('a Value.t -> unit) -> unit
      val addHandler: 'a t * (unit -> unit) -> unit
      val coerce: {clone: 'a -> 'a,
                   coerce: {from: 'a, to: 'a} -> unit,
                   equals: 'a * 'a -> bool} -> {from: 'a t, to: 'a t} -> unit
      val equals: 'a t * 'a t -> bool
      val getPoint: 'a t -> 'a Point.t option
      val isBottom: 'a t -> bool
      val isPoint: 'a t -> bool
      val isPointEq: ('a * 'a -> bool) -> 'a t * 'a Point.t -> bool
      val isTop: 'a t -> bool
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val lowerBound: {clone: 'a -> 'a,
                       coerce: {from: 'a, to: 'a} -> unit,
                       equals: 'a * 'a -> bool} -> 'a t * 'a Value.t -> unit
      val lowerBoundPoint: {clone: 'a -> 'a,
                            coerce: {from: 'a, to: 'a} -> unit,
                            equals: 'a * 'a -> bool} -> 'a t * 'a Point.t -> unit
      val makeTop: 'a t -> unit
      val new: 'a Value.t -> 'a t
      val newBottom: unit -> 'a t
      val newPoint: 'a Point.t -> 'a t
      val newTop: unit -> 'a t
      val unify: {clone: 'a -> 'a,
                  coerce: {from: 'a, to: 'a} -> unit,
                  equals: 'a * 'a -> bool,
                  unify: 'a * 'a -> unit} -> 'a t * 'a t -> unit
      val value: 'a t -> 'a Value.t
   end

signature FLAT_LATTICE_POLY_STRUCTS =
   sig
      structure Point:
         sig
            type 'a t

            val clone: {clone: 'a -> 'a,
                        equals: 'a * 'a -> bool} -> 'a t -> 'a t
            val equals: ('a * 'a -> bool) -> 'a t * 'a t -> bool
            val layout: ('a -> Layout.t) -> 'a t -> Layout.t
         end
      (* pretty print names *)
      val bottom: string
      val top: string
   end

signature FLAT_LATTICE_PARAM_STRUCTS =
   sig
      structure Point:
         sig
            type 'a t

            val equals: 'a t * 'a t -> bool
            val layout: ('a -> Layout.t) -> 'a t -> Layout.t
         end
      (* pretty print names *)
      val bottom: string
      val top: string
   end

signature FLAT_LATTICE_MONO_STRUCTS =
   sig
      structure Point:
         sig
            type t

            val equals: t * t -> bool
            val layout: t -> Layout.t
         end
      (* pretty print names *)
      val bottom: string
      val top: string
   end
