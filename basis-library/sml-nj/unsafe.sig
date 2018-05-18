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

      (* omit Size check;
       * elements have indeterminate value
       *)
      val create: int -> array
      (* omit Subscript check *)
      val sub: array * int -> elem
      (* omit Subscript check *)
      val update: array * int * elem -> unit
   end

(* SML/NJ provides 'create' and 'update',
 * but they are not provided with MLton,
 * because vectors are immutable and optimizations may
 * break if they are updated.
 *)
signature UNSAFE_MONO_VECTOR =
   sig
      type elem
      type vector

      (* omit Size check;
       * elements have indeterminate values *)
      (* val create: int -> vector *)
      (* omit Subscript check *)
      val sub: vector * int -> elem
      (* omit Subscript check *)
      (* val update: vector * int * elem -> unit *)
   end

signature UNSAFE =
   sig
      structure Array:
         sig
            (* omit Size check;
             * objptr(s) at elements set to bogus non-objptr value;
             * non-objptr(s) at elements have indeterminate value
             *)
            val alloc: int -> 'a array
            (* omit Size check;
             * elements set to initial value
             *)
            val create: int * 'a -> 'a array
            (* omit Subscript check *)
            val sub: 'a array * int -> 'a
            val uninitIsNop: 'a array -> bool
            (* omit Subscript check;
             * objptr(s) at element set to bogus non-objptr value
             *)
            val uninit: 'a array * int -> unit
            (* omit Subscript check *)
            val update: 'a array * int * 'a -> unit

            structure Raw:
               sig
                  type 'a rawarr

                  (* omit Size check;
                   * objptr(s) at elements have indeterminate value;
                   * non-objptr(s) at elements have indeterminate value
                   *)
                  val alloc: int -> 'a rawarr
                  (* prereq: all objptr(s) at elements set to bogus
                   * non-objptr value (via uninit)
                   *)
                  val toArray: 'a rawarr -> 'a array
                  val uninitIsNop: 'a rawarr -> bool
                  (* omit Subscript check;
                   * objptr(s) at element set to bogus non-objptr value
                   *)
                  val uninit: 'a rawarr * int -> unit
               end
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
            (* val create: int * 'a list -> 'a vector *)
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
