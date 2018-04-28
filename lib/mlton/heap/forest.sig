(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FOREST_HEAP_STRUCTS =
   sig
      structure Key: BOUNDED_ORDER
   end

signature FOREST_HEAP =
   sig
      include FOREST_HEAP_STRUCTS

      structure Elt:
         sig
            type 'a t
            val key: 'a t -> Key.t
            val value: 'a t -> 'a
         end

      type 'a t

      val empty: unit -> 'a t
      val insertLazy: 'a t * Key.t * 'a -> 'a Elt.t
      val insertEager: 'a t * Key.t * 'a -> 'a Elt.t
      val isEmpty: 'a t -> bool
      val deleteMin: 'a t -> 'a
      val decreaseKeySift: 'a t * 'a Elt.t * Key.t -> unit
      val decreaseKeyCut: 'a t * 'a Elt.t * Key.t -> unit
      val deleteSift: 'a t * 'a Elt.t -> unit
      val deleteCut: 'a t * 'a Elt.t -> unit
      val min: 'a t -> 'a Elt.t
      val newEager:  (Key.t * 'a) list -> 'a t
      val newLazy: (Key.t * 'a) list -> 'a t
      (* unions second heap into first, destroys second heap *)
      val unionLazy: 'a t * 'a t -> unit
      val unionEager: 'a t * 'a t -> unit
   end
