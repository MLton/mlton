(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoArray2 (V: CONCRETE_MONO_VECTOR): MONO_ARRAY2 =
   struct
      type elem = V.elem
      type vector = V.vector
      open Array2
      type array = elem array
      type region = {base: array,
		     row: int,
		     col: int,
		     nrows: int option,
		     ncols: int option}
   end

structure Word8Array2 = MonoArray2 (Word8Vector)

structure CharArray2: MONO_ARRAY2 =
   struct
      type elem = char
      type vector = CharVector.vector
      open Array2
      type array = elem array
      type region = {base: array,
		     row: int,
		     col: int,
		     nrows: int option,
		     ncols: int option}
      val row = Primitive.String.fromCharVector o row
      val column = Primitive.String.fromCharVector o column
   end

structure BoolArray2 = MonoArray2 (BoolVector)
structure IntArray2 = MonoArray2 (IntVector)
structure Int32Array2 = IntArray2
structure RealArray2 = MonoArray2 (RealVector)
structure Real64Array2 = RealArray2
structure WordArray2 = MonoArray2 (WordVector)
structure Word32Array2 = WordArray2

