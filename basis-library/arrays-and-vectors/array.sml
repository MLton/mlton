(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Array: ARRAY_EXTRA =
   struct
      structure A = Sequence (type 'a sequence = 'a array
                              type 'a elt = 'a
                              val fromArray = fn a => a
                              val isMutable = true
                              val length = Primitive.Array.length
                              val subUnsafe = Primitive.Array.subUnsafe)
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
            fun update' (arr, i, x) = 
               updateMk' Primitive.Array.updateUnsafe (arr, i, x)
            fun update (arr, i, x) = 
               updateMk Primitive.Array.updateUnsafe (arr, i, x)
            fun unsafeUpdate' (arr, i, x) = 
               unsafeUpdateMk' Primitive.Array.updateUnsafe (arr, i, x)
            fun unsafeUpdate (arr, i, x) = 
               unsafeUpdateMk Primitive.Array.updateUnsafe (arr, i, x)
            fun vector sl = create Vector.tabulate' (fn x => x) sl
            fun modifyi' f sl =
               appi' (fn (i, x) => unsafeUpdate' (sl, i, f (i, x))) sl
            fun modifyi f sl = modifyi' (wrap2 f) sl
            fun modify f sl = modifyi (f o #2) sl
            local
               fun make (length, sub') {src, dst, di} =
                  modifyi' (fn (i, _) => sub' (src, i)) 
                           (slice (dst, di, SOME (length src)))
            in
               fun copy (arg as {src, dst, di}) =
                  let val (src', si', len') = base src
                  in
                    if src' = dst andalso si' < di andalso di <= si' +? len' 
                       then let val sl = slice (dst, di, SOME (length src))
                            in 
                               foldri' (fn (i, _, _) => 
                                        unsafeUpdate' (sl, i, unsafeSub' (src, i)))
                               () sl
                            end
                    else make (length, unsafeSub') arg
                  end

               fun copyVec arg =
                  make (Vector.VectorSlice.length, Vector.VectorSlice.unsafeSub') arg
            end
         end

      local
        fun make f arr = f (ArraySlice.full arr)
      in
        fun vector arr = make (ArraySlice.vector) arr
        (* fun modifyi' f = make (ArraySlice.modifyi' f) *)
        fun modifyi f = make (ArraySlice.modifyi f)
        fun modify f = make (ArraySlice.modify f)
        fun copy {src, dst, di} = ArraySlice.copy {src = ArraySlice.full src,
                                                   dst = dst, di = di}
        fun copyVec {src, dst, di} = ArraySlice.copyVec {src = VectorSlice.full src,
                                                         dst = dst, di = di}
      end

      val arrayUninit' = newUninit'
      val arrayUninit = newUninit
      val array' = new'
      val array = new

      (* fun update' (arr, i, x) = updateMk' Primitive.Array.updateUnsafe (arr, i, x) *)
      fun update (arr, i, x) = updateMk Primitive.Array.updateUnsafe (arr, i, x)
      fun unsafeUpdate' (arr, i, x) = unsafeUpdateMk' Primitive.Array.updateUnsafe (arr, i, x)
      fun unsafeUpdate (arr, i, x) = unsafeUpdateMk Primitive.Array.updateUnsafe (arr, i, x)
   end
structure ArraySlice: ARRAY_SLICE_EXTRA = Array.ArraySlice

structure ArrayGlobal: ARRAY_GLOBAL = Array
open ArrayGlobal
