(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Word8Array = MonoArray (type elem = Word8.word
				  structure V = Word8Vector)
structure Word8ArraySlice = Word8Array.MonoArraySlice
structure CharArray = MonoArray(type elem = char
				structure V = CharVector)
structure CharArraySlice = CharArray.MonoArraySlice

structure BoolArray = MonoArray (type elem = bool
				 structure V = BoolVector)
structure BoolArraySlice = BoolArray.MonoArraySlice
structure IntArray = MonoArray (type elem = int
				structure V = IntVector)
structure IntArraySlice = IntArray.MonoArraySlice
structure Int32Array = IntArray
structure Int32ArraySlice = Int32Array.MonoArraySlice
structure RealArray = MonoArray (type elem = real
				 structure V = RealVector)
structure RealArraySlice = RealArray.MonoArraySlice
structure Real64Array = RealArray
structure Real64ArraySlice = Real64Array.MonoArraySlice
structure WordArray = MonoArray (type elem = word
				 structure V = WordVector)
structure WordArraySlice = WordArray.MonoArraySlice
structure Word32Array = WordArray
structure Word32ArraySlice = Word32Array.MonoArraySlice
