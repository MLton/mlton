(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature RESIZABLE_ARRAY =
   sig
      include ARRAY

      val addToEnd: 'a t * 'a -> unit
      val deleteLast: 'a t -> 'a
      val empty: unit -> 'a t
      val subOption: 'a t * int -> 'a option
   end
