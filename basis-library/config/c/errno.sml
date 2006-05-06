(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure C_Errno :>
   sig
      type 'a t
      val check: 'a t -> 'a
      val inject: 'a -> 'a t
   end =
   struct
      type 'a t = 'a
      val check = fn x => x
      val inject = fn x => x
   end
