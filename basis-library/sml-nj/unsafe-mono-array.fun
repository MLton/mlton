(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor UnsafeMonoArray(type elem): UNSAFE_MONO_ARRAY =
   struct
      type array = elem array
      type elem = elem

      val sub = Primitive.Array.sub
      val update = Primitive.Array.update
      val create = Primitive.Array.array
   end
   
