(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Char *)
structure CharArray = MonoArray(type elem = char
				structure V = CharVector)
structure CharArraySlice = CharArray.MonoArraySlice

(* Bool *)
structure BoolArray = MonoArray (type elem = bool
				 structure V = BoolVector)
structure BoolArraySlice = BoolArray.MonoArraySlice

(* Int *)
structure IntArray = MonoArray (type elem = int
				structure V = IntVector)
structure IntArraySlice = IntArray.MonoArraySlice
structure Int32Array = IntArray
structure Int32ArraySlice = IntArraySlice
structure Int16Array = MonoArray (type elem = Int16.int
				structure V = Int16Vector)
structure Int16ArraySlice = Int16Array.MonoArraySlice
structure Int8Array = MonoArray (type elem = Int8.int
				 structure V = Int8Vector)
structure Int8ArraySlice = Int8Array.MonoArraySlice

(* Real *)
structure RealArray = MonoArray (type elem = real
				 structure V = RealVector)
structure RealArraySlice = RealArray.MonoArraySlice
structure Real64Array = RealArray
structure Real64ArraySlice = Real64Array.MonoArraySlice

(* Word *)
structure WordArray = MonoArray (type elem = word
				 structure V = WordVector)
structure WordArraySlice = WordArray.MonoArraySlice
structure Word32Array = WordArray
structure Word32ArraySlice = Word32Array.MonoArraySlice
structure Word16Array = MonoArray (type elem = Word16.word
				   structure V = Word16Vector)
structure Word16ArraySlice = Word16Array.MonoArraySlice
structure Word8Array = MonoArray (type elem = Word8.word
				  structure V = Word8Vector)
structure Word8ArraySlice = Word8Array.MonoArraySlice
