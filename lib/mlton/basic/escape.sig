(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ESCAPE =
   sig
      type 'a t

      val escape: 'a t * 'a -> 'b
      val new: ('a t -> 'a) -> 'a
   end
