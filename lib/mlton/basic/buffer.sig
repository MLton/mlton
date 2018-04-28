(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BUFFER_STRUCTS = 
   sig
   end

signature BUFFER = 
   sig
      include BUFFER_STRUCTS

      type 'a t

      val add: 'a t * 'a -> unit
      val last: 'a t -> 'a option
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val length: 'a t -> int
      val new: {dummy: 'a} -> 'a t
      val reset: 'a t -> unit
      val toVector: 'a t -> 'a vector
   end
