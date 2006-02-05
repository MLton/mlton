(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Vector: VECTOR_EXTRA =
   struct
      structure V = Sequence (type 'a sequence = 'a vector
                              type 'a elt = 'a
                              val fromArray = Primitive.Vector.fromArray
                              val isMutable = false
                              val length = Primitive.Vector.length
                              val sub = Primitive.Vector.sub)
      open V

      type 'a vector = 'a vector

      structure VectorSlice = 
         struct
            open Slice
            type 'a vector = 'a vector
            val vector = sequence

            val isSubvector = isSubsequence
            val span = fn (sl, sl') => 
               span (op = : ''a vector * ''a vector -> bool) (sl, sl')
         end

      fun update (v, i, x) = 
        tabulate (length v,
                  fn j => if i = j 
                             then x
                          else unsafeSub (v, j))

      val unsafeSub = Primitive.Vector.sub

      val isSubvector = isSubsequence

      val fromArray = Primitive.Vector.fromArray

      val vector = new
   end
structure VectorSlice: VECTOR_SLICE_EXTRA = Vector.VectorSlice

structure VectorGlobal: VECTOR_GLOBAL = Vector
open VectorGlobal
val vector = Vector.fromList
