(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure BoolArray2 = MonoArray2 (type elem = bool
				   structure V = BoolVector)
structure CharArray2 = MonoArray2 (type elem = char
				   structure V = CharVector)
structure IntArray2 = MonoArray2 (type elem = int
				  structure V = IntVector)
structure Int32Array2 = IntArray2
structure RealArray2 = MonoArray2 (type elem = real
				   structure V = RealVector)
structure Real64Array2 = RealArray2
structure WordArray2 = MonoArray2 (type elem = word
				   structure V = WordVector)
structure Word8Array2 = MonoArray2 (type elem = Word8.word
				    structure V = Word8Vector)
structure Word32Array2 = WordArray2
