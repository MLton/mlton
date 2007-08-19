(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INT_SIZE_STRUCTS =
   sig
   end

signature INT_SIZE =
   sig
      include INT_SIZE_STRUCTS

      type t

      val all: t list
      val bits: t -> Bits.t
      val equals: t * t -> bool
      val fromBits : Bits.t -> t
      val memoize: (t -> 'a) -> t -> 'a
   end
