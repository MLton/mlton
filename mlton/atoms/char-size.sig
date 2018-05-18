(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHAR_SIZE_STRUCTS = 
   sig
   end

signature CHAR_SIZE = 
   sig
      include CHAR_SIZE_STRUCTS

      datatype t = C8 | C16 | C32

      val all: t list
      val bits: t -> Bits.t
      val equals: t * t -> bool
      val fromBits: Bits.t -> t
      val isInRange: t * IntInf.t -> bool
      val memoize: (t -> 'a) -> t -> 'a
   end
