(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor UnsafeMonoVector(type elem): UNSAFE_MONO_VECTOR =
   struct
      type vector = elem vector
      type elem = elem
      val sub = Primitive.Vector.sub
   end
