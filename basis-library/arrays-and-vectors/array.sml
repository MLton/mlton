(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Array: ARRAY_EXTRA =
   struct
      structure A = Sequence (type 'a sequence = 'a array
			      type 'a elt = 'a
			      val fromArray = fn a => a
			      val isMutable = true
			      val length = Primitive.Array.length
			      val sub = Primitive.Array.sub)
      open A
      open Primitive.Int

      type 'a array = 'a array
      type 'a vector = 'a Vector.vector

      structure ArraySlice =
	 struct
	    open Slice
	    fun update (arr, i, x) = 
	       update' Primitive.Array.update (arr, i, x)
	    fun unsafeUpdate (arr, i, x) = 
	       unsafeUpdate' Primitive.Array.update (arr, i, x)
	    fun vector sl = create Vector.tabulate (fn x => x) sl
	    fun modifyi f sl = 
	       appi (fn (i, x) => unsafeUpdate (sl, i, f (i, x))) sl
	    fun modify f sl = modifyi (f o #2) sl
	    local
	       fun make (length, sub) {src, dst, di} =
		  modifyi (fn (i, _) => sub (src, i)) 
		          (slice (dst, di, SOME (length src)))
	    in
	       fun copy (arg as {src, dst, di}) =
		  let val (src', si', len') = base src
		  in
		    if src' = dst andalso si' < di andalso si' +? len' >= di
		       then let val sl = slice (dst, di, SOME (length src))
			    in 
			       foldri (fn (i, _, _) => 
				       unsafeUpdate (sl, i, unsafeSub (src, i)))
			       () sl
			    end
		    else make (length, unsafeSub) arg
		  end

	       fun copyVec arg =
		  make (Vector.VectorSlice.length, Vector.VectorSlice.unsafeSub) arg
	    end
	 end

      val rawArray = Primitive.Array.array
      val array = new

      local
	fun make f arr = f (ArraySlice.full arr)
      in
	fun vector arr = make (ArraySlice.vector) arr
	fun modifyi f = make (ArraySlice.modifyi f)
	fun modify f = make (ArraySlice.modify f)
	fun copy {src, dst, di} = ArraySlice.copy {src = ArraySlice.full src,
						   dst = dst, di = di}
	fun copyVec {src, dst, di} = ArraySlice.copyVec {src = VectorSlice.full src,
							 dst = dst, di = di}
      end

      val unsafeSub = Primitive.Array.sub
      fun update (arr, i, x) = update' Primitive.Array.update (arr, i, x)
      val unsafeUpdate = Primitive.Array.update
   end
structure ArraySlice: ARRAY_SLICE_EXTRA = Array.ArraySlice

structure ArrayGlobal: ARRAY_GLOBAL = Array
open ArrayGlobal
