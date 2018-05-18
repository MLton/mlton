(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Vector: VECTOR_EXTRA =
   struct
      structure V = Sequence (Primitive.Vector)
      open V

      type 'a vector = 'a vector

      structure VectorSlice = 
         struct
            open Slice
            type 'a vector = 'a vector
            val vector = sequence

            val isSubvector = isSubsequence
            val span = fn (sl, sl') => 
               Primitive.Vector.Slice.span 
                  (op = : ''a vector * ''a vector -> bool) 
                  (sl, sl')
         end

      fun update (v, i, x) = 
         (Primitive.Vector.updateVector (v, SeqIndex.fromInt i, x))
         handle Overflow => raise Subscript

      val isSubvector = isSubsequence

      val unsafeFromArray = Primitive.Vector.unsafeFromArray

      val vector = new
   end
structure VectorSlice: VECTOR_SLICE_EXTRA = Vector.VectorSlice

structure VectorGlobal: VECTOR_GLOBAL = Vector
open VectorGlobal
val vector = Vector.fromList
