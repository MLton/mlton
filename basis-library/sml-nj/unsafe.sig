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

      (* val create: int -> vector *)
      val sub: vector * int -> elem
      (* val update: vector * int * elem -> unit *)
   end

signature UNSAFE =
   sig
      structure Array:
         sig
            val create: int * 'a -> 'a array
            val sub: 'a array * int -> 'a
            val update: 'a array * int * 'a -> unit
         end
      structure BoolArray: UNSAFE_MONO_ARRAY
      structure BoolVector: UNSAFE_MONO_VECTOR
      structure CharArray: UNSAFE_MONO_ARRAY
      structure CharVector: UNSAFE_MONO_VECTOR
      structure IntArray: UNSAFE_MONO_ARRAY
      structure IntVector: UNSAFE_MONO_VECTOR
      structure Int8Array: UNSAFE_MONO_ARRAY
      structure Int8Vector: UNSAFE_MONO_VECTOR
      structure Int16Array: UNSAFE_MONO_ARRAY
      structure Int16Vector: UNSAFE_MONO_VECTOR
      structure Int32Array: UNSAFE_MONO_ARRAY
      structure Int32Vector: UNSAFE_MONO_VECTOR
      structure Int64Array: UNSAFE_MONO_ARRAY
      structure Int64Vector: UNSAFE_MONO_VECTOR
      structure IntInfArray: UNSAFE_MONO_ARRAY
      structure IntInfVector: UNSAFE_MONO_VECTOR
      structure LargeIntArray: UNSAFE_MONO_ARRAY
      structure LargeIntVector: UNSAFE_MONO_VECTOR
      structure LargeRealArray: UNSAFE_MONO_ARRAY
      structure LargeRealVector: UNSAFE_MONO_VECTOR
      structure LargeWordArray: UNSAFE_MONO_ARRAY
      structure LargeWordVector: UNSAFE_MONO_VECTOR
      structure RealArray: UNSAFE_MONO_ARRAY
      structure RealVector: UNSAFE_MONO_VECTOR
      structure Real32Array: UNSAFE_MONO_ARRAY
      structure Real32Vector: UNSAFE_MONO_VECTOR
      structure Real64Array: UNSAFE_MONO_ARRAY
      structure Real64Vector: UNSAFE_MONO_VECTOR
      structure Vector:
         sig
            (* val create: int -> 'a vector *)
            val sub: 'a vector * int -> 'a
         end
      structure WordArray: UNSAFE_MONO_ARRAY
      structure WordVector: UNSAFE_MONO_VECTOR
      structure Word8Array: UNSAFE_MONO_ARRAY
      structure Word8Vector: UNSAFE_MONO_VECTOR
      structure Word16Array: UNSAFE_MONO_ARRAY
      structure Word16Vector: UNSAFE_MONO_VECTOR
      structure Word32Array: UNSAFE_MONO_ARRAY
      structure Word32Vector: UNSAFE_MONO_VECTOR
      structure Word64Array: UNSAFE_MONO_ARRAY
      structure Word64Vector: UNSAFE_MONO_VECTOR
      
      structure PackReal32Big : PACK_REAL
      structure PackReal32Little : PACK_REAL
      structure PackReal64Big : PACK_REAL
      structure PackReal64Little : PACK_REAL
      structure PackRealBig : PACK_REAL
      structure PackRealLittle : PACK_REAL
      structure PackWord16Big : PACK_WORD
      structure PackWord16Little : PACK_WORD
      structure PackWord32Big : PACK_WORD
      structure PackWord32Little : PACK_WORD
      structure PackWord64Big : PACK_WORD
      structure PackWord64Little : PACK_WORD
   end
