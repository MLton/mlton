(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure String0 = 
   struct
      open CharVector
      type char = elem
      type string = vector
      structure Substring0 =
	 struct
	    open CharVectorSlice
	    type char = elem
	    type string = vector
	    type substring = slice
	 end
      val maxSize = maxLen
      val size = length
      fun extract (s, start, len) = 
	 CharVectorSlice.vector (CharVectorSlice.slice (s, start, len))
      fun substring (s, start, len) = extract (s, start, SOME len)
      val op ^ = append
      val new = vector
      fun str c = new (1, c)
      val implode = fromList
      val explode = toList
   end
structure Substring0 = String0.Substring0
