(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure InsertionSort: INSERTION_SORT =
struct

open Array

(* Based on page 108 of Programming Pearls, by Bentley. *)
fun sort (a: 'a array, op <= : 'a * 'a -> bool): unit =
   let
      fun x i = sub (a, i)
      val _ =
	 Int.for
	 (1, Array.length a, fn i =>
	  let
	     val _ =
		Assert.assert ("insertionSort1", fn () =>
			       Array.isSortedRange (a, 0, i, op <=))
	     val t = x i
	     fun sift (j: int) =
		(Assert.assert
		 ("insertionSort2", fn () =>
		  Array.isSortedRange (a, 0, j, op <=)
		  andalso Array.isSortedRange (a, j + 1, i + 1, op <=)
		  andalso Int.forall (j + 1, i + 1, fn k =>
				      t <= x k))
		 ; if j > 0
		      then
			 let
			    val j' = j - 1
			    val z = x j'
			 in if t <= z
			       then (update (a, j, z)
				     ; sift j')
			    else j
			 end
		   else j)
	     val _ = update (a, sift i, t)
	  in ()
	  end)
      val _ =
	 Assert.assert ("insertionSort3", fn () => isSorted (a, op <=))
   in
      ()
   end

end
