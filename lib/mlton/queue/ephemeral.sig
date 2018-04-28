(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EPHEMERAL_QUEUE =
   sig
      type 'a t
      val isEmpty: 'a t -> bool
      val enque: 'a t * 'a -> unit
      val deque: 'a t -> 'a
   end
