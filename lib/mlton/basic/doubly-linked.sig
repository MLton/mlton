(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DOUBLY_LINKED_STRUCTS =
   sig
      type 'a t

      val destruct : 'a t -> 'a t Pointer.t * 'a * 'a t Pointer.t
   end

signature DOUBLY_LINKED =
   sig
      include DOUBLY_LINKED_STRUCTS

      val eqPrev: 'a t * 'a t -> bool
      val insertL: 'a t * 'a t -> unit
      val insertR: 'a t * 'a t -> unit
      val isLinked: 'a t -> bool
      val link: 'a t * 'a t -> unit
      val next: 'a t -> 'a t
      val nextp: 'a t -> 'a t Pointer.t
      val prev: 'a t -> 'a t
      val prevp: 'a t -> 'a t Pointer.t
      val setPrev: 'a t * 'a t -> unit
      val setNext: 'a t * 'a t -> unit
      val unlink: 'a t -> unit
      val value: 'a t -> 'a
   end
