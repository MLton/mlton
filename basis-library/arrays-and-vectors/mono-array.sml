(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoArray (V: CONCRETE_MONO_VECTOR): MONO_ARRAY =
   struct
      open Array
      type elem = V.elem
      type array = elem array
      type vector = V.vector
   end

structure Word8Array = MonoArray (Word8Vector)

(* Can't use MonoArray to create CharArray because Basis Library spec requires
 * type CharVector.vector = string, not char vector.
 *)
structure CharArray: MONO_ARRAY =
   struct
      open Array
      type elem = char
      type array = elem array
      type vector = CharVector.vector
      val vector = Primitive.String.fromCharVector o vector
      fun copyVec {src, dst, di} =
	 Array.copyVec {src = Primitive.String.toCharVector src,
			dst = dst, di = di}

      (* Depreciated *)
      val extract = Primitive.String.fromCharVector o extract
   end

structure BoolArray = MonoArray (BoolVector)
structure IntArray = MonoArray (IntVector)
structure Int32Array = IntArray
structure RealArray = MonoArray (RealVector)
structure Real64Array = RealArray
structure WordArray = MonoArray (WordVector)
structure Word32Array = WordArray