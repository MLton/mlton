(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature EPHEMERAL_QUEUE =
   sig
      type 'a t
      val isEmpty: 'a t -> bool
      val enque: 'a t * 'a -> unit
      val deque: 'a t -> 'a
   end
