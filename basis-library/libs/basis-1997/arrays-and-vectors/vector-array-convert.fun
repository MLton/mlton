(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)


functor VectorArrayConvert
        (structure Vector: VECTOR 
         structure VectorSlice: VECTOR_SLICE 
                                where type 'a slice = 'a VectorSlice.slice
         structure Array: ARRAY 
         structure ArraySlice: ARRAY_SLICE
                               where type 'a slice = 'a ArraySlice.slice) :
        sig
           structure Vector: VECTOR_1997
           structure Array: ARRAY_1997
        end =
  struct
     fun shift1 f (_, s, _) = fn (i:int, x) => f (i + s, x)
     fun shift2 f (_, s, _) = fn (i:int, x, y) => f (i + s, x, y)

     structure V =
       struct
          open Vector
          fun extract sl = VectorSlice.vector (VectorSlice.slice sl)
          fun mapi f sl = VectorSlice.mapi (shift1 f sl) (VectorSlice.slice sl)
          fun appi f sl = VectorSlice.appi (shift1 f sl) (VectorSlice.slice sl)
          fun foldli f b sl = VectorSlice.foldli (shift2 f sl) b (VectorSlice.slice sl)
          fun foldri f b sl = VectorSlice.foldri (shift2 f sl) b (VectorSlice.slice sl)
       end
     structure A = 
       struct
          open Array
          fun appi f sl = ArraySlice.appi (shift1 f sl) (ArraySlice.slice sl)
          fun copy {src, si, len, dst, di} =
            ArraySlice.copy {src = ArraySlice.slice (src, si, len),
                             dst = dst, di = di}
          fun copyVec {src, si, len, dst, di} =
            ArraySlice.copyVec {src = VectorSlice.slice (src, si, len),
                                dst = dst, di = di}
          fun extract sl = ArraySlice.vector (ArraySlice.slice sl)
          fun foldli f b sl = ArraySlice.foldli (shift2 f sl) b (ArraySlice.slice sl)
          fun foldri f b sl = ArraySlice.foldri (shift2 f sl) b (ArraySlice.slice sl)
          fun modifyi f sl = ArraySlice.modifyi (shift1 f sl) (ArraySlice.slice sl)
       end
     structure Vector = V
     structure Array = A
  end
