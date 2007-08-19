(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EQ_MONO =
   sig
      structure Array: MONO_ARRAY_EXTRA
      structure Array2: MONO_ARRAY2
      structure ArraySlice: MONO_ARRAY_SLICE_EXTRA
      structure Vector: EQTYPE_MONO_VECTOR_EXTRA
      structure VectorSlice: EQTYPE_MONO_VECTOR_SLICE_EXTRA
      sharing type Array.array = ArraySlice.array = Vector.array
      sharing type Array.elem = Array2.elem = ArraySlice.elem = Vector.elem
         = VectorSlice.elem
      sharing type Array.vector = Array2.vector = ArraySlice.vector
         = Vector.vector = VectorSlice.vector
      sharing type ArraySlice.vector_slice = VectorSlice.slice
   end

functor EqMono (eqtype elem) =
   struct
      structure Vector = EqtypeMonoVector (type elem = elem)
      structure VectorSlice = Vector.MonoVectorSlice
      structure Array = MonoArray (type elem = elem
                                   structure V = Vector)
      structure ArraySlice = Array.MonoArraySlice
      structure Array2 = MonoArray2 (type elem = elem
                                     structure V = Vector)
   end

functor Mono (type elem) =
   struct
      structure Vector = MonoVector (type elem = elem)
      structure VectorSlice = Vector.MonoVectorSlice
      structure Array = MonoArray (type elem = elem
                                   structure V = Vector)
      structure ArraySlice = Array.MonoArraySlice
      structure Array2 = MonoArray2 (type elem = elem
                                     structure V = Vector)
   end

local
   structure S = EqMono (type elem = Primitive.Bool.bool)
   open S
in
   structure BoolVector = Vector
   structure BoolVectorSlice = VectorSlice
   structure BoolArray = Array
   structure BoolArraySlice = ArraySlice
   structure BoolArray2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Int8.int)
   open S
in
   structure Int8Vector = Vector
   structure Int8VectorSlice = VectorSlice
   structure Int8Array = Array
   structure Int8ArraySlice = ArraySlice
   structure Int8Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Int16.int)
   open S
in
   structure Int16Vector = Vector
   structure Int16VectorSlice = VectorSlice
   structure Int16Array = Array
   structure Int16ArraySlice = ArraySlice
   structure Int16Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Int32.int)
   open S
in
   structure Int32Vector = Vector
   structure Int32VectorSlice = VectorSlice
   structure Int32Array = Array
   structure Int32ArraySlice = ArraySlice
   structure Int32Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Int64.int)
   open S
in
   structure Int64Vector = Vector
   structure Int64VectorSlice = VectorSlice
   structure Int64Array = Array
   structure Int64ArraySlice = ArraySlice
   structure Int64Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.IntInf.int)
   open S
in
   structure IntInfVector = Vector
   structure IntInfVectorSlice = VectorSlice
   structure IntInfArray = Array
   structure IntInfArraySlice = ArraySlice
   structure IntInfArray2 = Array2
end
local
   structure S = Mono (type elem = Primitive.Real32.real)
   open S
in
   structure Real32Vector = Vector
   structure Real32VectorSlice = VectorSlice
   structure Real32Array = Array
   structure Real32ArraySlice = ArraySlice
   structure Real32Array2 = Array2
end
local
   structure S = Mono (type elem = Primitive.Real64.real)
   open S
in
   structure Real64Vector = Vector
   structure Real64VectorSlice = VectorSlice
   structure Real64Array = Array
   structure Real64ArraySlice = ArraySlice
   structure Real64Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Word8.word)
   open S
in
   structure Word8Vector = Vector
   structure Word8VectorSlice = VectorSlice
   structure Word8Array = Array
   structure Word8ArraySlice = ArraySlice
   structure Word8Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Word16.word)
   open S
in
   structure Word16Vector = Vector
   structure Word16VectorSlice = VectorSlice
   structure Word16Array = Array
   structure Word16ArraySlice = ArraySlice
   structure Word16Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Word32.word)
   open S
in
   structure Word32Vector = Vector
   structure Word32VectorSlice = VectorSlice
   structure Word32Array = Array
   structure Word32ArraySlice = ArraySlice
   structure Word32Array2 = Array2
end
local
   structure S = EqMono (type elem = Primitive.Word64.word)
   open S
in
   structure Word64Vector = Vector
   structure Word64VectorSlice = VectorSlice
   structure Word64Array = Array
   structure Word64ArraySlice = ArraySlice
   structure Word64Array2 = Array2
end


local
   structure S = EqMono (type elem = Char.char)
   open S
in
   structure CharArray = Array
   structure CharArray2 = Array2
   structure CharArraySlice = ArraySlice
   structure CharVector = Vector
   structure CharVectorSlice = VectorSlice
end
local
   structure S = EqMono (type elem = WideChar.char)
   open S
in
   structure WideCharArray = Array
   structure WideCharArray2 = Array2
   structure WideCharArraySlice = ArraySlice
   structure WideCharVector = Vector
   structure WideCharVectorSlice = VectorSlice
end
local
   structure S = EqMono (type elem = Int.int)
   open S
in
   structure IntVector = Vector
   structure IntVectorSlice = VectorSlice
   structure IntArray = Array
   structure IntArraySlice = ArraySlice
   structure IntArray2 = Array2
end
local
   structure S = EqMono (type elem = LargeInt.int)
   open S
in
   structure LargeIntVector = Vector
   structure LargeIntVectorSlice = VectorSlice
   structure LargeIntArray = Array
   structure LargeIntArraySlice = ArraySlice
   structure LargeIntArray2 = Array2
end
local
   structure S = Mono (type elem = Real.real)
   open S
in
   structure RealVector = Vector
   structure RealVectorSlice = VectorSlice
   structure RealArray = Array
   structure RealArraySlice = ArraySlice
   structure RealArray2 = Array2
end
local
   structure S = Mono (type elem = LargeReal.real)
   open S
in
   structure LargeRealVector = Vector
   structure LargeRealVectorSlice = VectorSlice
   structure LargeRealArray = Array
   structure LargeRealArraySlice = ArraySlice
   structure LargeRealArray2 = Array2
end
local
   structure S = EqMono (type elem = Word.word)
   open S
in
   structure WordVector = Vector
   structure WordVectorSlice = VectorSlice
   structure WordArray = Array
   structure WordArraySlice = ArraySlice
   structure WordArray2 = Array2
end
local
   structure S = EqMono (type elem = LargeWord.word)
   open S
in
   structure LargeWordVector = Vector
   structure LargeWordVectorSlice = VectorSlice
   structure LargeWordArray = Array
   structure LargeWordArraySlice = ArraySlice
   structure LargeWordArray2 = Array2
end
