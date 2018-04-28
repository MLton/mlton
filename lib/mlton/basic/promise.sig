(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROMISE =
   sig
      type 'a t

      exception Force

      val delay: (unit -> 'a) -> 'a t
      val force: 'a t -> 'a
      val isUnevaluated: 'a t -> bool
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val lazy: (unit -> 'a) -> (unit -> 'a)
      val reset: 'a t * (unit -> 'a) -> unit
   end
