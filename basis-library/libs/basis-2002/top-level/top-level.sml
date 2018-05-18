(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

open Basis2002

(* Rebind some structures so that their definitions appear later, so that they
 * will be used for displaying tycon names.
 *
 * Order here matters!  Do not alphabetize or otherwise reorder without thinking.
 *)
structure Posix = Posix
structure OS = OS
structure BoolArray = BoolArray
structure BoolArray2 = BoolArray2
structure BoolVector = BoolVector
structure CharArraySlice = CharArraySlice
structure CharArray = CharArray
structure CharArray2 = CharArray2
structure Int8Array = Int8Array
structure Int8Array2 = Int8Array2
structure Int8Vector = Int8Vector
structure Int16Array = Int16Array
structure Int16Array2 = Int16Array2
structure Int16Vector = Int16Vector
structure Int32Array = Int32Array
structure Int32Array2 = Int32Array2
structure Int32Vector = Int32Vector
structure Int64Array = Int64Array
structure Int64Array2 = Int64Array2
structure Int64Vector = Int64Vector
structure IntArray = IntArray
structure IntArray2 = IntArray2
structure IntVector = IntVector
structure LargeIntArray = LargeIntArray
structure LargeIntArray2 = LargeIntArray2
structure LargeIntVector = LargeIntVector
structure LargeRealArray = LargeRealArray
structure LargeRealArray2 = LargeRealArray2
structure LargeRealVector = LargeRealVector
structure LargeWordArray = LargeWordArray
structure LargeWordArray2 = LargeWordArray2
structure LargeWordVector = LargeWordVector
structure Real32Array = Real32Array
structure Real32Array2 = Real32Array2
structure Real32Vector = Real32Vector
structure Real64Array = Real64Array
structure Real64Array2 = Real64Array2
structure Real64Vector = Real64Vector
structure RealArray = RealArray
structure RealArray2 = RealArray2
structure RealVector = RealVector
structure Word8Array = Word8Array
structure Word8Array2 = Word8Array2
structure Word8Vector = Word8Vector
structure Word16Array = Word16Array
structure Word16Array2 = Word16Array2
structure Word16Vector = Word16Vector
structure Word32Array = Word32Array
structure Word32Array2 = Word32Array2
structure Word32Vector = Word32Vector
structure Word64Array = Word64Array
structure Word64Array2 = Word64Array2
structure Word64Vector = Word64Vector
structure WordArray = WordArray
structure WordArray2 = WordArray2
structure WordVector = WordVector
structure Array = Array
structure Array2 = Array2
structure Vector = Vector
structure Int8 = Int8
structure Int16 = Int16
structure Int32 = Int32
structure Int64 = Int64
structure IntInf = IntInf
structure LargeInt = LargeInt
structure Int = Int
structure Real32 = Real32
structure Real64 = Real64
structure LargeReal = LargeReal
structure Real = Real
structure Word8 = Word8
structure Word16 = Word16
structure Word32 = Word32
structure Word64 = Word64
structure LargeWord = LargeWord
structure Word = Word
