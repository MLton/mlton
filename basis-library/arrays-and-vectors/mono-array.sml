(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoArray (V: CONCRETE_MONO_VECTOR_EXTRA): MONO_ARRAY_EXTRA =
   struct
      open Array
      type elem = V.elem
      type array = elem array
      type vector = elem vector
      type vector_slice = elem vector_slice
      structure MonoArraySlice =
	 struct
	    open ArraySlice
	    type elem = elem
	    type array = elem array
	    type slice = elem slice
	    type vector = elem vector
	    type vector_slice = elem vector_slice
	 end
   end

structure Word8Array = MonoArray (Word8Vector)
structure Word8ArraySlice = Word8Array.MonoArraySlice

(* Can't use MonoArray to create CharArray because Basis Library spec requires
 * type CharVector.vector = string, not char vector.
 *)
structure CharArray: MONO_ARRAY_EXTRA =
   struct
      open Array
      type elem = char
      type array = elem array
      type vector = CharVector.vector
      type vector_slice = CharVectorSlice.slice
      structure MonoArraySlice =
	 struct
	    open ArraySlice
	    type elem = char
	    type array = elem array
	    type slice = elem slice
	    type vector = CharVector.vector
	    type vector_slice = CharVectorSlice.slice
	    val vector = Primitive.String.fromCharVector o vector
	    fun copyVec {src: vector_slice, dst: array, di: int} =
	       let 
		  val (s, start, len) = CharVectorSlice.base src
		  val s = Primitive.String.toCharVector s
		  val src = VectorSlice.unsafeSlice (s, start, SOME len)
	       in
		  ArraySlice.copyVec {src = src, dst = dst, di = di}
	       end
	 end
      val vector = Primitive.String.fromCharVector o vector
      fun copyVec {src, dst, di} =
	 Array.copyVec {src = Primitive.String.toCharVector src,
			dst = dst, di = di}

      (* Depreciated *)
      val extract = Primitive.String.fromCharVector o extract
   end
structure CharArraySlice = CharArray.MonoArraySlice

structure BoolArray = MonoArray (BoolVector)
structure BoolArraySlice = BoolArray.MonoArraySlice
structure IntArray = MonoArray (IntVector)
structure IntArraySlice = IntArray.MonoArraySlice
structure Int32Array = IntArray
structure Int32ArraySlice = Int32Array.MonoArraySlice
structure RealArray = MonoArray (RealVector)
structure RealArraySlice = RealArray.MonoArraySlice
structure Real64Array = RealArray
structure Real64ArraySlice = Real64Array.MonoArraySlice
structure WordArray = MonoArray (WordVector)
structure WordArraySlice = WordArray.MonoArraySlice
structure Word32Array = WordArray
structure Word32ArraySlice = Word32Array.MonoArraySlice
