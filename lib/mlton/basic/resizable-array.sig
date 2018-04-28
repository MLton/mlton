(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RESIZABLE_ARRAY =
   sig
      include ARRAY

      val addToEnd: 'a t * 'a -> unit
      val deleteLast: 'a t -> 'a
      val empty: unit -> 'a t
      val subOption: 'a t * int -> 'a option
   end
