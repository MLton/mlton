(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ARRAY_STRUCTS =
   sig
      include VECTOR_STRUCTS

      val update: 'a t * int * 'a -> unit
   end

signature ARRAY =
   sig
      include VECTOR

      val array: int * 'a -> 'a t (* synonym for new *)
      val fromVector: 'a vector -> 'a t
      val getAndSet: 'a t -> (int -> 'a) * (int * 'a -> unit)
      val modify: 'a t * ('a -> 'a) -> unit
      val shuffle: 'a t -> unit
      (* Put random elements in the first n positions. *)
      val shuffleN: 'a t * int -> unit
      val swap: 'a t * int * int -> unit
      val toVector: 'a t -> 'a vector
      val toVectorMap: 'a t * ('a -> 'b) -> 'b vector
      val update: 'a t * int * 'a -> unit
   end
