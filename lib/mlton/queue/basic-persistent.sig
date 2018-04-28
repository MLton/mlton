(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BASIC_PERSISTENT_QUEUE =
   sig
      type 'a t

      val empty: unit -> 'a t
      val isEmpty: 'a t -> bool
      val destruct: 'a t -> ('a * 'a t) option
      val enque: 'a t * 'a -> 'a t
   end
