(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_WEAK =
   sig
      type 'a t

      val get: 'a t -> 'a option
      val new: 'a -> 'a t
   end
