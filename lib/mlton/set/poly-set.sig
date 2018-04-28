(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature POLY_SET =
   sig
      structure I: INTEGER

      type 'a t

      val empty: {equal: 'a * 'a -> bool,
                   output: 'a * Out.t -> unit} -> 'a t

      val size: 'a t -> I.t
      val foreach: 'a t * ('a -> unit) -> unit
      val forall: 'a t * ('a -> bool) -> bool
      val exists: 'a t * ('a -> bool) -> bool
      val equals: 'a t * 'a t -> bool
      val <: 'a t * 'a t -> bool
      val <=: 'a t * 'a t -> bool
      val >: 'a t * 'a t -> bool
      val >=: 'a t * 'a t -> bool

      val +: 'a t * 'a t -> 'a t
      val -: 'a t * 'a t -> 'a t
      val intersect: 'a t * 'a t -> 'a t

(*      val union: 'a t list -> 'a t*)
      val subset: 'a t * ('a -> bool) -> 'a t

      val add: 'a t * 'a -> 'a t
      val remove: 'a t * 'a -> 'a t

      val contains: 'a t * 'a -> bool
      val isEmpty: 'a t -> bool

      val toList: 'a t -> 'a list

      val output: 'a t * Out.t -> unit
   end
