(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoVector(type elem): MONO_VECTOR_EXTRA =
   struct
      open Vector
      type elem = elem
      type vector = elem vector
      structure MonoVectorSlice = 
	 struct
	    open VectorSlice
	    type elem = elem
	    type vector = elem vector
	    type slice = elem slice
	 end
   end

structure Word8Vector = MonoVector(type elem = Word8.word)
structure Word8VectorSlice = Word8Vector.MonoVectorSlice

(* Basis Library spec requires type CharVector.vector = string *)
structure CharVector =
   struct
      open String0
      type elem = char
      type vector = string
      structure MonoVectorSlice =
	 struct
	    open String0Slice
	    type elem = elem
	    type vector = vector
	    type slice = Substring.substring
	 end
   end
structure CharVectorSlice = CharVector.MonoVectorSlice

structure BoolVector = MonoVector(type elem = bool)
structure BoolVectorSlice = BoolVector.MonoVectorSlice
structure IntVector = MonoVector(type elem = int)
structure IntVectorSlice = IntVector.MonoVectorSlice
structure Int32Vector = IntVector
structure Int32VectorSlice = Int32Vector.MonoVectorSlice
structure RealVector = MonoVector(type elem = real)
structure RealVectorSlice = RealVector.MonoVectorSlice
structure Real64Vector = RealVector
structure Real64VectorSlice = Real64Vector.MonoVectorSlice
structure WordVector = MonoVector(type elem = word)
structure WordVectorSlice = WordVector.MonoVectorSlice
structure Word32Vector = WordVector
structure Word32VectorSlice = Word32Vector.MonoVectorSlice
