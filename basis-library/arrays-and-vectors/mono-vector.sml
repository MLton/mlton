(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Char *)
(* Moved to text/string0.sml
structure CharVector = MonoVector(type elem = char)
structure CharVectorSlice = CharVector.MonoVectorSlice
*)

(* Bool *)
structure BoolVector = EqtypeMonoVector(type elem = bool)
structure BoolVectorSlice = BoolVector.MonoVectorSlice

(* Int *)
structure IntVector = EqtypeMonoVector(type elem = int)
structure IntVectorSlice = IntVector.MonoVectorSlice
structure Int32Vector = IntVector
structure Int32VectorSlice = Int32Vector.MonoVectorSlice
structure Int16Vector = EqtypeMonoVector(type elem = Int16.int)
structure Int16VectorSlice = Int16Vector.MonoVectorSlice
structure Int8Vector = EqtypeMonoVector(type elem = Int8.int)
structure Int8VectorSlice = Int8Vector.MonoVectorSlice

(* Real *)
structure RealVector = MonoVector(type elem = real)
structure RealVectorSlice = RealVector.MonoVectorSlice
structure Real64Vector = RealVector
structure Real64VectorSlice = Real64Vector.MonoVectorSlice

(* Word *)
structure WordVector = EqtypeMonoVector(type elem = word)
structure WordVectorSlice = WordVector.MonoVectorSlice
structure Word32Vector = WordVector
structure Word32VectorSlice = Word32Vector.MonoVectorSlice
structure Word16Vector = EqtypeMonoVector(type elem = Word16.word)
structure Word16VectorSlice = Word16Vector.MonoVectorSlice
structure Word8Vector = EqtypeMonoVector(type elem = Word8.word)
structure Word8VectorSlice = Word8Vector.MonoVectorSlice
