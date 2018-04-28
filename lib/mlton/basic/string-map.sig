(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STRING_MAP = 
   sig
      type 'a t

      val clear: 'a t -> unit
      val domain: 'a t -> string list
      val foreach: 'a t * ('a -> unit) -> unit
      val keepAll: 'a t * ('a -> bool) -> string list
      val lookup: 'a t * string -> 'a
      val new: (unit -> 'a) -> 'a t
   end
