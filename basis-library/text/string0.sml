(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure CharVector = MonoVector(type elem = char)
structure CharVectorSlice = CharVector.MonoVectorSlice

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

      fun update (v, i, x) = 
	tabulate (length v,
		  fn j => if i = j 
			     then x
			  else unsafeSub (v, j))

      val size = length

      fun extract (s, i, j) = Substring0.copy (Substring0.extract (s, i, j))
      fun substring (s, i, j) = extract (s, i, SOME j)

      (* QUESTION: is it worth writing a concat2 that doesn't need to build and
       *  traverse a list?  concat is too complicated for knownCase to unroll it.
       *)
      fun s ^ s' = concat [s, s']
      fun concatWith sep ss =
	 (case ss of
	     [] => ""
	   | [s] => s
	   | s::ss => List.foldl (fn (s,res) => concat [res, sep, s]) s ss)

      fun implode cs =
	 let val a = Primitive.Array.array (List.length cs)
	 in List.foldl (fn (c, i) => (Array.update (a, i, c) ; i +? 1)) 0 cs ;
	    fromArray a
	 end

      fun new (l, c) = vector (l, c)
      fun str c = new (1, c)
   end
structure Substring0 = String0.Substring0