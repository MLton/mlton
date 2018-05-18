(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature QUEUE =
   sig
      type 'a t

      val deque: 'a t -> ('a t * 'a) option
      val empty: unit -> 'a t
      val enque: 'a t * 'a -> 'a t
      val foldAnyOrder: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldr: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val isEmpty: 'a t -> bool
      val toList: 'a t -> 'a list
   end
