(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature APPEND_LIST = 
   sig
      type 'a t

      val append: 'a t * 'a t -> 'a t
      val appends: 'a t list -> 'a t
      val appendsV: 'a t vector -> 'a t
      val cons: 'a * 'a t -> 'a t
      val empty: 'a t
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldr: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foreach: 'a t * ('a -> unit) -> unit
      val fromList: 'a list -> 'a t
      val fromVector: 'a vector -> 'a t
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val length: 'a t -> int
      val map: 'a t * ('a -> 'b) -> 'b t
      val push: 'a t ref * 'a -> unit
      val single: 'a -> 'a t
      val snoc: 'a t * 'a -> 'a t
      val toList: 'a t -> 'a list
      val toListOnto: 'a t * 'a list -> 'a list
      val toVector: 'a t -> 'a vector
   end
