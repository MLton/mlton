(* Copyright (C) 2009 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature UNSAFE =
   sig
      structure Array:
         sig
            val sub: 'a array * int -> 'a
            val update: 'a array * int * 'a -> unit
         end
      structure Vector:
         sig
            val sub: 'a vector * int -> 'a
         end
   end