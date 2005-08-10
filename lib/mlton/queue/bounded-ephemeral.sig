(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
