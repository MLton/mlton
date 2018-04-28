(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature POINTER =
   sig
      type 'a t

      val ! : 'a t -> 'a
      val := : 'a t * 'a -> unit

      val clear: 'a t -> unit
      val copy: 'a t * 'a t -> unit
      val eq: 'a t * 'a t -> bool
      val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool
      val follow: 'a t -> 'a option
      val isNull: 'a t -> bool
      val make: 'a option -> 'a t
      val null: unit -> 'a t
      val new: 'a -> 'a t      
      val swap: 'a t * 'a t -> unit
   end
