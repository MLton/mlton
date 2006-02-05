(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MonoVector (type elem): MONO_VECTOR_EXTRA 
                                where type elem = elem =
   struct
      open Vector
      type array = elem array
      type elem = elem
      type vector = elem vector
      structure MonoVectorSlice = 
         struct
            open VectorSlice
            type elem = elem
            type vector = vector
            type slice = elem slice
         end
   end

functor EqtypeMonoVector (eqtype elem): EQTYPE_MONO_VECTOR_EXTRA 
                                        where type elem = elem =
   struct
      open Vector
      type array = elem array
      type elem = elem
      type vector = elem vector
      val fromPoly = fn v => v
      val toPoly = fn v => v
      structure MonoVectorSlice = 
         struct
            open VectorSlice
            type elem = elem
            type vector = vector
            type slice = elem slice
            val fromPoly = fn s => s
            val toPoly = fn s => s
         end
   end
