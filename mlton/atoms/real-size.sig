(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REAL_SIZE_STRUCTS = 
   sig
   end

signature REAL_SIZE = 
   sig
      include REAL_SIZE_STRUCTS

      datatype t = R32 | R64

      val all: t list
      val bits: t -> Bits.t
      val bytes: t -> Bytes.t
      val equals: t * t -> bool
      val memoize: (t -> 'a) -> t -> 'a
      val toString: t -> string
   end
