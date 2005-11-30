(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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
structure BoolVector = BoolVector
structure CharArraySlice = CharArraySlice
structure CharArray = CharArray
structure Int8Array = Int8Array
structure Int8Vector = Int8Vector
structure Int16Array = Int16Array
structure Int16Vector = Int16Vector
structure Int32Array = Int32Array
structure Int32Vector = Int32Vector
structure Int64Array = Int64Array
structure Int64Vector = Int64Vector
structure IntArray = IntArray
structure IntVector = IntVector
structure LargeIntArray = LargeIntArray
structure LargeIntVector = LargeIntVector
structure LargeRealArray = LargeRealArray
structure LargeRealVector = LargeRealVector
structure LargeWordArray = LargeWordArray
structure LargeWordVector = LargeWordVector
structure Real32Array = Real32Array
structure Real32Vector = Real32Vector
structure Real64Array = Real64Array
structure Real64Vector = Real64Vector
structure RealArray = RealArray
structure RealVector = RealVector
structure Word8Array = Word8Array
structure Word8Vector = Word8Vector
structure Word16Array = Word16Array
structure Word16Vector = Word16Vector
structure Word32Array = Word32Array
structure Word32Vector = Word32Vector
structure Word64Array = Word64Array
structure Word64Vector = Word64Vector
structure WordArray = WordArray
structure WordVector = WordVector
structure Int8 = Int8
structure Int16 = Int16
structure Int32 = Int32
structure Int64 = Int64
structure IntInf = IntInf
structure Real32 = Real32
structure Real64 = Real64
structure Word8 = Word8
structure Word16 = Word16
structure Word32 = Word32
structure Word64 = Word64
