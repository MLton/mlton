(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure QuickSort: QUICK_SORT =
struct

open Array
   
val rand = Word.toIntX o MLton.Random.rand

fun randInt (lo, hi) = lo + Int.mod (rand(), hi - lo + 1)

(* quicksort based on page 112 of Programming Pearls, by Bentley.
 * It does the repeated partitioning until the segment size is less than
 * the cutoff.  Then, it does an insertion sort over the whole array to fix up
 * the unsorted segments.
 *)
fun 'a sortArray (a: 'a array, op <= : 'a * 'a -> bool): 'a array =
   let
      fun x i = sub (a, i)
      fun swap (i, j) =
	 let
	    val t = x i
	    val _ = update (a, i, x j)
	    val _ = update (a, j, t)
	 in ()
	 end
      val cutoff = 20
      fun qsort (l: int, u: int): unit =
	 if u - l > cutoff
	    then
	       let
		  val _ = swap (l, randInt (l, u))
		  val t = x l
		  val m =
		     Int.fold
		     (l + 1, u + 1, l, fn (i, m) =>
		      (if true
			  then ()
		       else
			  Assert.assert
			  ("QuickSort", fn () =>
			   Int.forall (l + 1, m + 1, fn k => x k <= t)
			   andalso Int.forall (m + 1, i, fn k => not (x k <= t)))
		       ; if x i <= t
			    then (swap (m + 1, i)
				  ; m + 1)
			 else m))
		  val _ = swap (l, m)
		  val _ = qsort (l, m - 1)
		  val _ = qsort (m + 1, u)
	       in ()
	       end
	 else ()
      val max = length a - 1
      val _ = qsort (0, max)
      val _ = InsertionSort.sort (a, op <=)
   in
      a
   end

fun sortList (l, f) =
   Array.toList (sortArray (Array.fromList l, f))

fun sortVector (v, f) =
   Array.toVector (sortArray (Array.fromVector v, f))
   
end
