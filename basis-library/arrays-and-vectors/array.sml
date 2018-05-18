(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Array: ARRAY_EXTRA =
   struct
      structure A = Sequence (Primitive.Array)
      open A

      val op +? = Int.+?
      val op < = Int.<
      val op <= = Int.<=

      fun wrap2 f = fn (i, x) => f (SeqIndex.toIntUnsafe i, x)

      type 'a array = 'a array
      type 'a vector = 'a Vector.vector

      structure ArraySlice =
         struct
            open Slice
            val vector = Primitive.Array.Slice.vector
            val copyVec = Vector.VectorSlice.copy
            val unsafeCopyVec = Vector.VectorSlice.unsafeCopy
            fun modifyi f sl = Primitive.Array.Slice.modifyi (wrap2 f) sl
            val modify = Primitive.Array.Slice.modify
         end

      val array = new
      val unsafeArray = unsafeNew
      val vector = Primitive.Array.vector
      val copyVec = Vector.copy
      val unsafeCopyVec = Vector.unsafeCopy
      fun modifyi f sl = Primitive.Array.modifyi (wrap2 f) sl
      val modify = Primitive.Array.modify

      structure Raw = Primitive.Array.Raw
      structure Raw =
         struct
            type 'a rawarr = 'a Raw.rawarr

            fun length a =
               if Primitive.Controls.safe
                  then (SeqIndex.toInt (Raw.length a))
                       handle Overflow => raise Fail "Raw.length"
                  else SeqIndex.toIntUnsafe (Raw.length a)

            fun alloc n = Raw.alloc (SeqIndex.fromIntForLength n)
            fun unsafeAlloc n = Raw.unsafeAlloc (SeqIndex.fromIntUnsafe n)

            val uninitIsNop = Raw.uninitIsNop
            fun unsafeUninit (a, i) =
               Raw.unsafeUninit (a, SeqIndex.fromIntUnsafe i)
            fun uninit (a, i) =
               if Primitive.Controls.safe
                  then let
                          val i =
                             (SeqIndex.fromInt i)
                             handle Overflow => raise Subscript
                       in
                          Raw.uninit (a, i)
                       end
                  else unsafeUninit (a, i)

            val unsafeToArray = Primitive.Array.Raw.unsafeToArray
         end
   end

structure ArraySlice: ARRAY_SLICE_EXTRA = Array.ArraySlice

structure ArrayGlobal: ARRAY_GLOBAL = Array
open ArrayGlobal
