(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor UnsafeMonoVector(type elem): UNSAFE_MONO_VECTOR =
   struct
      type vector = elem vector
      type elem = elem
      val sub = Primitive.Vector.sub
   end
