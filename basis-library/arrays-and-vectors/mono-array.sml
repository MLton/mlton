(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Word8Array = MonoArray (Word8Vector)
structure Word8ArraySlice = Word8Array.MonoArraySlice
structure CharArray = MonoArray(CharVector)
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
