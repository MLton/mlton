(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BOUNDED_EPHEMERAL_QUEUE =
   sig
      include EPHEMERAL_QUEUE

      structure I: INTEGER
      val empty: I.t -> '1a t
      val size: 'a t -> I.t
      val maxSize: '1a t -> I.t
      val isFull: '1a t -> bool
   end
