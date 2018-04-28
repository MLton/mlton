(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PERSISTENT_QUEUE =
   sig
      include BASIC_PERSISTENT_QUEUE

      val deque: 'a t -> 'a * 'a t
   end
