(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)


functor MonoVectorArrayArray2Convert
        (structure Vector: MONO_VECTOR
         structure VectorSlice: MONO_VECTOR_SLICE
         structure Array: MONO_ARRAY
         structure ArraySlice: MONO_ARRAY_SLICE
         structure Array2: MONO_ARRAY2
         sharing type Vector.elem = VectorSlice.elem =
                      Array.elem = ArraySlice.elem =
                      Array2.elem
         sharing type Vector.vector = VectorSlice.vector =
                      Array.vector = ArraySlice.vector = 
                      Array2.vector
         sharing type VectorSlice.slice = ArraySlice.vector_slice
         sharing type Array.array = ArraySlice.array) :
        sig
           structure Vector: MONO_VECTOR_1997
           structure Array: MONO_ARRAY_1997
           structure Array2: MONO_ARRAY2_1997
           sharing type Vector.elem = Array.elem = Array2.elem
           sharing type Vector.vector = Array.Vector.vector = Array2.Vector.vector
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
          structure Vector = V
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
     structure A2 =
       struct
          open Array2
          structure Vector = V
       end
     structure Array = A
     structure Vector = V
     structure Array2 = A2
  end
