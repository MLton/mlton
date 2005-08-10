(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor MonoArray (type elem
		   structure V: MONO_VECTOR_EXTRA
		     where type elem = elem
		       and type vector = elem Vector.vector
                       and type MonoVectorSlice.slice = elem VectorSlice.slice
                  ): MONO_ARRAY_EXTRA 
                     where type elem = elem
                       and type vector = V.vector
                       and type vector_slice = V.MonoVectorSlice.slice =
   struct
      open Array

      type elem = V.elem
      type array = elem array
      type vector = V.vector
      type vector_slice = V.MonoVectorSlice.slice

      val fromPoly = fn a => a
      val toPoly = fn a => a

      structure MonoArraySlice =
	 struct
	    open ArraySlice

	    type elem = elem
	    type array = array
	    type slice = elem slice
	    type vector = vector
	    type vector_slice = vector_slice

	    val toPoly = fn s => s
	 end
   end
