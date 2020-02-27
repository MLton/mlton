(* Copyright (C) 2009,2017,2019-2020 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROD_STRUCTS =
   sig
   end

signature PROD =
   sig
      type 'a t

      val allAreImmutable: 'a t -> bool
      val allAreMutable: 'a t -> bool
      val dest: 'a t -> {elt: 'a, isMutable: bool} vector
      val elt: 'a t * int -> 'a
      val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool
      val empty: unit -> 'a t
      val first: 'a t -> {elt: 'a, isMutable: bool}
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foreach: 'a t * ('a -> unit) -> unit
      val hash: 'a t * ('a -> word) -> word
      val isEmpty: 'a t -> bool
      val keepAllMap: 'a t * ('a -> 'b option) -> 'b t
      val layout: 'a t * ('a -> Layout.t) -> Layout.t
      val length: 'a t -> int
      val make: {elt: 'a, isMutable: bool} vector -> 'a t
      val map: 'a t * ('a -> 'b) -> 'b t
      val new1Immutable: 'a -> 'a t
      val new1Mutable: 'a -> 'a t
      val parse: 'a Parse.t -> 'a t Parse.t
      val someIsImmutable: 'a t -> bool
      val someIsMutable: 'a t -> bool
      val sub: 'a t * int -> {elt: 'a, isMutable: bool}
   end
