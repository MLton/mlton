(* Copyright (C) 1999-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

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
   structure S = EqMono (type elem = Bool.bool)
   open S
in
   structure BoolVector = Vector
   structure BoolVectorSlice = VectorSlice
   structure BoolArray = Array
   structure BoolArraySlice = ArraySlice
   structure BoolArray2 = Array2
end
local
   structure S = EqMono (type elem = Char.char)
   open S
in
   structure CharVector = Vector
   structure CharVectorSlice = VectorSlice
   structure CharArray = Array
   structure CharArraySlice = ArraySlice
   structure CharArray2 = Array2
end
local
   structure S = EqMono (type elem = Int8.int)
   open S
in
   structure Int8Vector = Vector
   structure Int8VectorSlice = VectorSlice
   structure Int8Array = Array
   structure Int8ArraySlice = ArraySlice
   structure Int8Array2 = Array2
end
local
   structure S = EqMono (type elem = Int16.int)
   open S
in
   structure Int16Vector = Vector
   structure Int16VectorSlice = VectorSlice
   structure Int16Array = Array
   structure Int16ArraySlice = ArraySlice
   structure Int16Array2 = Array2
end
local
   structure S = EqMono (type elem = Int32.int)
   open S
in
   structure Int32Vector = Vector
   structure Int32VectorSlice = VectorSlice
   structure Int32Array = Array
   structure Int32ArraySlice = ArraySlice
   structure Int32Array2 = Array2
end
local
   structure S = EqMono (type elem = Int64.int)
   open S
in
   structure Int64Vector = Vector
   structure Int64VectorSlice = VectorSlice
   structure Int64Array = Array
   structure Int64ArraySlice = ArraySlice
   structure Int64Array2 = Array2
end
local
   structure S = EqMono (type elem = IntInf.int)
   open S
in
   structure IntInfVector = Vector
   structure IntInfVectorSlice = VectorSlice
   structure IntInfArray = Array
   structure IntInfArraySlice = ArraySlice
   structure IntInfArray2 = Array2
end
local
   structure S = Mono (type elem = Real32.real)
   open S
in
   structure Real32Vector = Vector
   structure Real32VectorSlice = VectorSlice
   structure Real32Array = Array
   structure Real32ArraySlice = ArraySlice
   structure Real32Array2 = Array2
end
local
   structure S = Mono (type elem = Real64.real)
   open S
in
   structure Real64Vector = Vector
   structure Real64VectorSlice = VectorSlice
   structure Real64Array = Array
   structure Real64ArraySlice = ArraySlice
   structure Real64Array2 = Array2
end
local
   structure S = EqMono (type elem = Word8.word)
   open S
in
   structure Word8Vector = Vector
   structure Word8VectorSlice = VectorSlice
   structure Word8Array = Array
   structure Word8ArraySlice = ArraySlice
   structure Word8Array2 = Array2
end
local
   structure S = EqMono (type elem = Word16.word)
   open S
in
   structure Word16Vector = Vector
   structure Word16VectorSlice = VectorSlice
   structure Word16Array = Array
   structure Word16ArraySlice = ArraySlice
   structure Word16Array2 = Array2
end
local
   structure S = EqMono (type elem = Word32.word)
   open S
in
   structure Word32Vector = Vector
   structure Word32VectorSlice = VectorSlice
   structure Word32Array = Array
   structure Word32ArraySlice = ArraySlice
   structure Word32Array2 = Array2
end
local
   structure S = EqMono (type elem = Word64.word)
   open S
in
   structure Word64Vector = Vector
   structure Word64VectorSlice = VectorSlice
   structure Word64Array = Array
   structure Word64ArraySlice = ArraySlice
   structure Word64Array2 = Array2
end

structure IntVector = Int32Vector
structure IntVectorSlice = Int32VectorSlice
structure IntArray = Int32Array
structure IntArraySlice = Int32ArraySlice
structure IntArray2 = Int32Array2

structure LargeIntVector = IntInfVector
structure LargeIntVectorSlice = IntInfVectorSlice
structure LargeIntArray = IntInfArray
structure LargeIntArraySlice = IntInfArraySlice
structure LargeIntArray2 = IntInfArray2

structure RealVector = Real64Vector
structure RealVectorSlice = Real64VectorSlice
structure RealArray = Real64Array
structure RealArraySlice = Real64ArraySlice
structure RealArray2 = Real64Array2

structure LargeRealVector = Real64Vector
structure LargeRealVectorSlice = Real64VectorSlice
structure LargeRealArray = Real64Array
structure LargeRealArraySlice = Real64ArraySlice
structure LargeRealArray2 = Real64Array2

structure WordVector = Word32Vector
structure WordVectorSlice = Word32VectorSlice
structure WordArray = Word32Array
structure WordArraySlice = Word32ArraySlice
structure WordArray2 = Word32Array2

structure LargeWordVector = Word64Vector
structure LargeWordVectorSlice = Word64VectorSlice
structure LargeWordArray = Word64Array
structure LargeWordArraySlice = Word64ArraySlice
structure LargeWordArray2 = Word64Array2
