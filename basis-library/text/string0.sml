(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure String0 =
   struct
      val fromArray =
	 Primitive.String.fromCharVector o Primitive.Vector.fromArray

      structure S = Sequence (type 'a sequence = string
			      type 'a elt = char
			      val fromArray = fromArray
			      val isMutable = false
			      open Primitive.String
			      val length = size)
      open S

      open Primitive.Int
	 
      type string = string
      type array = string

      val maxSize = maxLen

      val size = length

      fun substring (s, i, j) = extract (s, i, SOME j)

      fun update (v, i, x) = 
	tabulate (length v,
		  fn j => if i = j 
			     then x
			  else unsafeSub (v, j))

      fun s ^ s' = concat [s, s']

      fun implode cs =
	 let val a = Primitive.Array.array (List.length cs)
	 in List.foldl (fn (c, i) => (Array.update (a, i, c) ; i +? 1)) 0 cs ;
	    fromArray a
	 end

      fun str c = new (1, c)
   end
