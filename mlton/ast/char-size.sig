(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHAR_SIZE_STRUCTS = 
   sig
   end

signature CHAR_SIZE = 
   sig
      include CHAR_SIZE_STRUCTS
      
      datatype t = C1 | C2 | C4

      val all: t list
      val bits: t -> Bits.t
      val default: t
      val equals: t * t -> bool
      val fromBits: Bits.t -> t
      val isInRange: t * IntInf.t -> bool
      val memoize: (t -> 'a) -> t -> 'a
   end
