(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature SCALE_STRUCTS =
   sig
   end

signature SCALE =
   sig
      include SCALE_STRUCTS
         
      datatype t = One | Two | Four | Eight

      val fromInt: int -> t option
      val layout: t -> Layout.t
      val toInt: t -> int
      val toString: t -> string
   end
