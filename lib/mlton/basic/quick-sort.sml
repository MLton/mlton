(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure QuickSort: QUICK_SORT =
struct

open Array
   
(* Check if entries in a[lo ... hi] are sorted. *)
fun 'a isSorted (a: 'a array,
		 lo: int,
		 hi: int,
		 op <= : 'a * 'a -> bool) =
   let
      fun loop (i, x) =
	 i > hi
	 orelse let
		   val y = sub (a, i)
		in
		   x <= y andalso loop (i + 1, y)
		end
   in
      lo >= hi orelse loop (lo + 1, sub (a, lo))
   end

(* From page 284 of Numerical Recipes in C. *)
local
   open Word
   val seed: word ref = ref 0w13
in
   fun rand () =
      let
	 val res = 0w1664525 * !seed + 0w1013904223
	 val _ = seed := res
      in
	 toIntX res
      end
end

fun randInt (lo, hi) = lo + Int.mod (rand(), hi - lo + 1)

(* quicksort based on page 112 of Programming Pearls, by Bentley.
 * It does the repeated partitioning until the segment size is less than
 * the cutoff.  Then, it does an insertion sort over the whole array to fix up
 * the unsorted segments.
 *)
fun 'a sort (a: 'a array, op <= : 'a * 'a -> bool): unit =
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
		      (Assert.assert
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
      ()
   end

end
