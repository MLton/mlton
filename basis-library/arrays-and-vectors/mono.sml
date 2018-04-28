(* Copyright (C) 2015 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EQTYPE_MONO =
   sig
      type elem
      structure MonoArray: MONO_ARRAY_EXTRA
      structure MonoArray2: MONO_ARRAY2
      structure MonoVector: EQTYPE_MONO_VECTOR_EXTRA
      sharing type MonoArray.array = MonoVector.array
      sharing type elem = MonoArray.elem = MonoArray2.elem = MonoVector.elem
      sharing type MonoArray.vector = MonoArray2.vector = MonoVector.vector
      sharing type MonoArray.MonoArraySlice.vector_slice = MonoVector.MonoVectorSlice.slice
   end

signature MONO =
   sig
      type elem
      structure MonoArray: MONO_ARRAY_EXTRA
      structure MonoArray2: MONO_ARRAY2
      structure MonoVector: MONO_VECTOR_EXTRA
      sharing type MonoArray.array = MonoVector.array
      sharing type elem = MonoArray.elem = MonoArray2.elem = MonoVector.elem
      sharing type MonoArray.vector = MonoArray2.vector = MonoVector.vector
      sharing type MonoArray.MonoArraySlice.vector_slice = MonoVector.MonoVectorSlice.slice
   end

functor EqtypeMonoX (eqtype elem) =
   struct
      type elem = elem
      structure MonoVector = EqtypeMonoVector (type elem = elem)
      structure MonoArray = MonoArray (type elem = elem
                                       structure MV = MonoVector)
      structure MonoArray2 = MonoArray2 (type elem = elem
                                         structure MV = MonoVector)
   end

functor EqtypeMono (eqtype elem) :> EQTYPE_MONO where type elem = elem =
   struct
      type elem = elem
      structure MonoVector = EqtypeMonoVector (type elem = elem)
      structure MonoArray = MonoArray (type elem = elem
                                       structure MV = MonoVector)
      structure MonoArray2 = MonoArray2 (type elem = elem
                                         structure MV = MonoVector)
   end

functor Mono (type elem) :> MONO where type elem = elem =
   struct
      type elem = elem
      structure MonoVector = MonoVector (type elem = elem)
      structure MonoArray = MonoArray (type elem = elem
                                       structure MV = MonoVector)
      structure MonoArray2 = MonoArray2 (type elem = elem
                                         structure MV = MonoVector)
   end

local
   structure S = EqtypeMono (type elem = Primitive.Bool.bool)
   open S
in
   structure BoolVector = MonoVector
   structure BoolVectorSlice = MonoVector.MonoVectorSlice
   structure BoolArray = MonoArray
   structure BoolArraySlice = MonoArray.MonoArraySlice
   structure BoolArray2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Int8.int)
   open S
in
   structure Int8Vector = MonoVector
   structure Int8VectorSlice = MonoVector.MonoVectorSlice
   structure Int8Array = MonoArray
   structure Int8ArraySlice = MonoArray.MonoArraySlice
   structure Int8Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Int16.int)
   open S
in
   structure Int16Vector = MonoVector
   structure Int16VectorSlice = MonoVector.MonoVectorSlice
   structure Int16Array = MonoArray
   structure Int16ArraySlice = MonoArray.MonoArraySlice
   structure Int16Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Int32.int)
   open S
in
   structure Int32Vector = MonoVector
   structure Int32VectorSlice = MonoVector.MonoVectorSlice
   structure Int32Array = MonoArray
   structure Int32ArraySlice = MonoArray.MonoArraySlice
   structure Int32Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Int64.int)
   open S
in
   structure Int64Vector = MonoVector
   structure Int64VectorSlice = MonoVector.MonoVectorSlice
   structure Int64Array = MonoArray
   structure Int64ArraySlice = MonoArray.MonoArraySlice
   structure Int64Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.IntInf.int)
   open S
in
   structure IntInfVector = MonoVector
   structure IntInfVectorSlice = MonoVector.MonoVectorSlice
   structure IntInfArray = MonoArray
   structure IntInfArraySlice = MonoArray.MonoArraySlice
   structure IntInfArray2 = MonoArray2
end
local
   structure S = Mono (type elem = Primitive.Real32.real)
   open S
in
   structure Real32Vector = MonoVector
   structure Real32VectorSlice = MonoVector.MonoVectorSlice
   structure Real32Array = MonoArray
   structure Real32ArraySlice = MonoArray.MonoArraySlice
   structure Real32Array2 = MonoArray2
end
local
   structure S = Mono (type elem = Primitive.Real64.real)
   open S
in
   structure Real64Vector = MonoVector
   structure Real64VectorSlice = MonoVector.MonoVectorSlice
   structure Real64Array = MonoArray
   structure Real64ArraySlice = MonoArray.MonoArraySlice
   structure Real64Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Word8.word)
   open S
in
   structure Word8Vector = MonoVector
   structure Word8VectorSlice = MonoVector.MonoVectorSlice
   structure Word8Array = MonoArray
   structure Word8ArraySlice = MonoArray.MonoArraySlice
   structure Word8Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Word16.word)
   open S
in
   structure Word16Vector = MonoVector
   structure Word16VectorSlice = MonoVector.MonoVectorSlice
   structure Word16Array = MonoArray
   structure Word16ArraySlice = MonoArray.MonoArraySlice
   structure Word16Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Word32.word)
   open S
in
   structure Word32Vector = MonoVector
   structure Word32VectorSlice = MonoVector.MonoVectorSlice
   structure Word32Array = MonoArray
   structure Word32ArraySlice = MonoArray.MonoArraySlice
   structure Word32Array2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Primitive.Word64.word)
   open S
in
   structure Word64Vector = MonoVector
   structure Word64VectorSlice = MonoVector.MonoVectorSlice
   structure Word64Array = MonoArray
   structure Word64ArraySlice = MonoArray.MonoArraySlice
   structure Word64Array2 = MonoArray2
end


local
   structure S = EqtypeMonoX (type elem = Char.char)
   open S
in
   structure CharVector = MonoVector
   structure CharVectorSlice = MonoVector.MonoVectorSlice
   structure CharArray = MonoArray
   structure CharArraySlice = MonoArray.MonoArraySlice
   structure CharArray2 = MonoArray2
end
local
   structure S = EqtypeMonoX (type elem = WideChar.char)
   open S
in
   structure WideCharVector = MonoVector
   structure WideCharVectorSlice = MonoVector.MonoVectorSlice
   structure WideCharArray = MonoArray
   structure WideCharArraySlice = MonoArray.MonoArraySlice
   structure WideCharArray2 = MonoArray2
end

local
   structure S = EqtypeMono (type elem = Int.int)
   open S
in
   structure IntVector = MonoVector
   structure IntVectorSlice = MonoVector.MonoVectorSlice
   structure IntArray = MonoArray
   structure IntArraySlice = MonoArray.MonoArraySlice
   structure IntArray2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = LargeInt.int)
   open S
in
   structure LargeIntVector = MonoVector
   structure LargeIntVectorSlice = MonoVector.MonoVectorSlice
   structure LargeIntArray = MonoArray
   structure LargeIntArraySlice = MonoArray.MonoArraySlice
   structure LargeIntArray2 = MonoArray2
end
local
   structure S = Mono (type elem = Real.real)
   open S
in
   structure RealVector = MonoVector
   structure RealVectorSlice = MonoVector.MonoVectorSlice
   structure RealArray = MonoArray
   structure RealArraySlice = MonoArray.MonoArraySlice
   structure RealArray2 = MonoArray2
end
local
   structure S = Mono (type elem = LargeReal.real)
   open S
in
   structure LargeRealVector = MonoVector
   structure LargeRealVectorSlice = MonoVector.MonoVectorSlice
   structure LargeRealArray = MonoArray
   structure LargeRealArraySlice = MonoArray.MonoArraySlice
   structure LargeRealArray2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = Word.word)
   open S
in
   structure WordVector = MonoVector
   structure WordVectorSlice = MonoVector.MonoVectorSlice
   structure WordArray = MonoArray
   structure WordArraySlice = MonoArray.MonoArraySlice
   structure WordArray2 = MonoArray2
end
local
   structure S = EqtypeMono (type elem = LargeWord.word)
   open S
in
   structure LargeWordVector = MonoVector
   structure LargeWordVectorSlice = MonoVector.MonoVectorSlice
   structure LargeWordArray = MonoArray
   structure LargeWordArraySlice = MonoArray.MonoArraySlice
   structure LargeWordArray2 = MonoArray2
end
