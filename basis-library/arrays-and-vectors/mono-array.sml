(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoArray (V: CONCRETE_MONO_VECTOR): MONO_ARRAY =
   struct
      structure Vector = V
      type elem = V.elem
      open Array
      type array = elem array
   end

structure Word8Array = MonoArray (Word8Vector)
(* Can't use MonoArray to create CharArray because Basis Library spec requires
 * type CharVector.vector = string, not char vector.
 *)
structure CharArray: MONO_ARRAY =
   struct
      structure Vector = CharVector
      type elem = char
      open Array
      type array = elem array
      val extract = Primitive.String.fromCharVector o extract
      fun copyVec {src, dst, si, len, di} =
	 Array.copyVec {src = Primitive.String.toCharVector src,
			dst = dst, si = si, len = len, di = di}
   end
structure BoolArray = MonoArray (BoolVector)
structure IntArray = MonoArray (IntVector)
structure RealArray = MonoArray (RealVector)
structure Real64Array = RealArray
