(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Word8Vector = MonoVector(type elem = Word8.word)
structure Word8VectorSlice = Word8Vector.MonoVectorSlice
(* Moved to string0.sml
structure CharVector = MonoVector(type elem = char)
structure CharVectorSlice = CharVector.MonoVectorSlice
*)

structure BoolVector = MonoVector(type elem = bool)
structure BoolVectorSlice = BoolVector.MonoVectorSlice
structure IntVector = MonoVector(type elem = int)
structure IntVectorSlice = IntVector.MonoVectorSlice
structure Int32Vector = IntVector
structure Int32VectorSlice = Int32Vector.MonoVectorSlice
structure RealVector = MonoVector(type elem = real)
structure RealVectorSlice = RealVector.MonoVectorSlice
structure Real64Vector = RealVector
structure Real64VectorSlice = Real64Vector.MonoVectorSlice
structure WordVector = MonoVector(type elem = word)
structure WordVectorSlice = WordVector.MonoVectorSlice
structure Word32Vector = WordVector
structure Word32VectorSlice = Word32Vector.MonoVectorSlice
