(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoArray (V: CONCRETE_MONO_VECTOR_EXTRA): MONO_ARRAY_EXTRA =
   struct
      open Array
      type elem = V.elem
      type array = elem array
      type vector = elem vector
      type vector_slice = elem vector_slice
      structure MonoArraySlice =
	 struct
	    open ArraySlice
	    type elem = elem
	    type array = elem array
	    type slice = elem slice
	    type vector = elem vector
	    type vector_slice = elem vector_slice
	 end
   end
