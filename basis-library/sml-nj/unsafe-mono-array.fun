(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor UnsafeMonoArray(type elem): UNSAFE_MONO_ARRAY =
   struct
      type array = elem array
      type elem = elem

      val sub = Primitive.Array.sub
      val update = Primitive.Array.update
      val create = Primitive.Array.array
   end
   
