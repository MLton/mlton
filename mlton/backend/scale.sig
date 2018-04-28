(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SCALE_STRUCTS =
   sig
   end

signature SCALE =
   sig
      include SCALE_STRUCTS

      datatype t = One | Two | Four | Eight

      val fromBytes: Bytes.t -> t option
      val layout: t -> Layout.t
      val toInt: t -> int
      val toString: t -> string
   end
