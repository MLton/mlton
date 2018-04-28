(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CLEARABLE_PROMISE =
   sig
      type 'a t

      exception Force

      val clear: 'a t -> unit
      val delay: (unit -> 'a) -> 'a t
      val force: 'a t -> 'a
   end
