(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
structure RealVector = MonoVector(type elem = real)

