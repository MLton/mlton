(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHAR_BUFFER_STRUCTS = 
   sig
   end

signature CHAR_BUFFER = 
   sig
      include CHAR_BUFFER_STRUCTS

      type t

      val new: unit -> t
      val length: t -> int
      val reset: t -> unit
      val toString: t -> string
      val addChar: t * char -> unit
      val layout: t -> Layout.t
   end
