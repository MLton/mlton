(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoVector(type elem): MONO_VECTOR =
   struct
      open Vector
      type elem = elem
      type vector = elem vector
   end

structure Word8Vector = MonoVector(type elem = Word8.word)

(* Basis Library spec requires type CharVector.vector = string *)
structure CharVector =
   struct
      open String0
      type vector = string
      type elem = char
   end

structure BoolVector = MonoVector(type elem = bool)
structure IntVector = MonoVector(type elem = int)
structure Int32Vector = IntVector
structure RealVector = MonoVector(type elem = real)
structure Real64Vector = RealVector
structure WordVector = MonoVector(type elem = word)
structure Word32Vector = WordVector
