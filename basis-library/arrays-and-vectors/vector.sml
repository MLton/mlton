(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
      val vector = new

      structure VectorSlice = 
	 struct
	    open Slice
	    type 'a vector = 'a vector
	    fun vector sl = copy sl
	 end

      fun update (v, i, x) = 
	tabulate (length v,
		  fn j => if i = j 
			     then x
			  else unsafeSub (v, j))

      val fromArray = Primitive.Vector.fromArray
      val unsafeSub = Primitive.Vector.sub
   end
structure VectorSlice: VECTOR_SLICE_EXTRA = Vector.VectorSlice

structure VectorGlobal: VECTOR_GLOBAL = Vector
open VectorGlobal
val vector = Vector.fromList
