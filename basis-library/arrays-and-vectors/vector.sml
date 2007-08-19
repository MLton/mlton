(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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
                              val subUnsafe = Primitive.Vector.subUnsafe)
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
         let
            fun doit i =
               tabulate' (length' v,
                          fn j => if i = j 
                                     then x 
                                     else unsafeSub' (v, j))
         in 
            if Primitive.Controls.safe
               then
                  let
                     val i = 
                        (SeqIndex.fromInt i)
                        handle Overflow => raise Subscript
                  in
                     if SeqIndex.geu (i, length' v)
                        then raise Subscript
                        else doit i
                  end 
               else let
                       val i = SeqIndex.fromIntUnsafe i
                    in 
                       doit i
                    end
         end

      val isSubvector = isSubsequence

      val unsafeFromArray = Primitive.Vector.fromArray

      val vector = new

      val create = generate
   end
structure VectorSlice: VECTOR_SLICE_EXTRA = Vector.VectorSlice

structure VectorGlobal: VECTOR_GLOBAL = Vector
open VectorGlobal
val vector = Vector.fromList
