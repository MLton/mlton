(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature HEAP_STRUCTS =
   sig
      structure Key: BOUNDED_ORDER
   end

signature HEAP =
   sig
      include HEAP_STRUCTS

      structure Elt:
         sig
            type 'a t

            val key: 'a t -> Key.t
            val value: 'a t -> 'a
         end

      type 'a t

      val decreaseKey: 'a t * 'a Elt.t * Key.t -> unit
      val delete: 'a t * 'a Elt.t -> unit
      val deleteMin: 'a t -> 'a
      val empty: unit -> 'a t
      val insert: 'a t * Key.t * 'a -> 'a Elt.t
      val isEmpty: 'a t -> bool
      val min: 'a t -> 'a Elt.t
      val new: (Key.t * 'a) list -> 'a t
      (* union(h, h') unions h' into h, destroying h'. *)
      val union: 'a t * 'a t -> unit
   end
