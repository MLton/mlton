(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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

      fun copy s = tabulate (length s, fn i => sub (s, i))
	 
      fun map f s =
	 fromArray (Array.tabulate (size s, fn i => f (sub (s, i))))

      fun s ^ s' = concat [s, s']

      fun implode cs =
	 let val a = Primitive.Array.array (List.length cs)
	 in List.foldl (fn (c, i) => (Array.update (a, i, c) ; i +? 1)) 0 cs ;
	    fromArray a
	 end

      fun str c = new (1, c)
   end
