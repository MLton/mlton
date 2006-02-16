(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature UNBOUNDED_EPHEMERAL_QUEUE =
   sig
      include EPHEMERAL_QUEUE
      
      val empty: unit -> 'a t
   end
