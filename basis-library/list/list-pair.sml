(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure ListPair: LIST_PAIR =
   struct
      fun unzip l =
	 List.foldr (fn ((x, y), (xs, ys)) => (x :: xs, y :: ys)) ([], []) l

      fun foldl f b (l1, l2) =
	 let
	    fun loop(l1, l2, b) =
	       case (l1, l2) of
		  (x1 :: l1, x2 :: l2) => loop(l1, l2, f(x1, x2, b))
		| _ => b
	 in loop(l1, l2, b)
	 end

      fun foldr f b (l1, l2) =
	 let
	    fun loop(l1, l2) =
	       case (l1, l2) of
		  (x1 :: l1, x2 :: l2) => f(x1, x2, loop(l1, l2))
		| _ => b
	 in loop(l1, l2)
	 end

      fun zip(l1, l2) = rev(foldl (fn (x, x', l) => (x, x') :: l) [] (l1, l2))
	 
      fun map f = rev o (foldl (fn (x1, x2, l) => f(x1, x2) :: l) [])
	 
      fun app f = foldl (fn (x1, x2, ()) => f(x1, x2)) ()

      fun exists p (l1, l2) =
	 let
	    fun loop(l1, l2) =
	       case (l1, l2) of
		  (x1 :: l1, x2 :: l2) => p(x1, x2) orelse loop(l1, l2)
		| _ => false
	 in loop(l1, l2)
	 end

      fun all p ls = not(exists (not o p) ls)
   end
