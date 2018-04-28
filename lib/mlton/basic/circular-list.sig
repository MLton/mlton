(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CIRCULAR_LIST_STRUCTS =
   sig
      structure Elt: DOUBLY_LINKED
   end

signature CIRCULAR_LIST =
   sig
      include CIRCULAR_LIST_STRUCTS

      type 'a t = 'a Elt.t Pointer.t

      val delete: 'a t * 'a Elt.t -> unit
      val deleteEach: 'a t * ('a Elt.t -> unit) -> unit
      val empty: unit -> 'a t
      val first: 'a t -> 'a Elt.t
      val foreach: 'a t * ('a Elt.t -> unit) -> unit
      val insert: 'a t * 'a Elt.t -> unit
      val rotate: 'a t -> unit
      val splice: 'a t * 'a t -> unit
   end
