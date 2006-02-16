(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PID =
   sig
      type t
         
      val current: unit -> t
      val equals: t * t -> bool
      val fromString: string -> t option
      val layout: t -> Layout.t
      val parent: unit -> t
      val toString: t -> string
   end
