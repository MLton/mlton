(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure CharVector = EqtypeMonoVector(type elem = char)
structure CharVectorSlice = CharVector.MonoVectorSlice
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
      fun s1 ^ s2 = concat [s1, s2]
      val new = vector
      fun str c = new (1, c)
      val implode = fromList
      val explode = toList
   end
structure Substring0 = String0.Substring0

(*
structure String0 =
   struct
      open CharVector
      open Primitive.Int
	 
      type char = elem
      type string = vector

      structure Substring0 = 
	 struct
	    open CharVectorSlice
	    type char = elem
	    type string = vector
	    type substring = slice
	    val extract = slice
	    val unsafeExtract = unsafeSlice
	    fun substring (s, i, j) = extract (s, i, SOME j)
	    fun unsafeSubstring (s, i, j) = unsafeExtract (s, i, SOME j)
	 end

      val maxSize = maxLen
      val size = length

      fun extract (s, i, j) = Substring0.sequence (Substring0.extract (s, i, j))
      fun substring (s, i, j) = extract (s, i, SOME j)

      (* QUESTION: is it worth writing a concat2 that doesn't need to build and
       *  traverse a list?  concat is too complicated for knownCase to unroll it.
       *)
      fun s ^ s' = concat [s, s']

      val implode = fromString
      fun str c = new (1, c)
   end
structure Substring0 = String0.Substring0
*)