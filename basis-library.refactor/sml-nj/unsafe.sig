(* A subset of the UNSAFE signature provided by SML/NJ.  Modified from SML/NJ
 * sources, which are
 *
 * Copyright (c) 1997 Bell Labs, Lucent Technologies.
 *
 *)

signature UNSAFE_MONO_ARRAY =
   sig
      type array
      type elem

      val create: int -> array
      val sub: array * int -> elem
      val update: array * int * elem -> unit
   end

(* sweeks took out update and create because vectors are immutable
 * and mlton optimizations may break if you update them.
 *)
signature UNSAFE_MONO_VECTOR =
   sig
      type elem
      type vector

      val sub: vector * int -> elem
   end

signature UNSAFE =
   sig
      structure Array:
         sig
            val create: int * 'a -> 'a array
            val sub: 'a array * int -> 'a
            val update: 'a array * int * 'a -> unit
         end
      structure CharArray: UNSAFE_MONO_ARRAY
      structure CharVector: UNSAFE_MONO_VECTOR
      structure Real64Array: UNSAFE_MONO_ARRAY
      structure Vector:
         sig
            val sub: 'a vector * int -> 'a
         end
      structure Word8Array: UNSAFE_MONO_ARRAY
      structure Word8Vector: UNSAFE_MONO_VECTOR
   end
