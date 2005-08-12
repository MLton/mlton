(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure QuickSort: QUICK_SORT =
struct

open Array

type int = Int.int

val rand = Word.toIntX o MLton.Random.rand

fun randInt (lo, hi) = lo + Int.mod (rand(), hi - lo + 1)

(* quicksort based on section 10.2 of Programming Pearls, by Bentley.
 * It does repeated partitioning until the segment size is less than the cutoff.
 * Then, it does an insertion sort over the whole array to fix up the unsorted
 * segments.
 *)
fun 'a sortArray (a: 'a array, op <= : 'a * 'a -> bool): 'a array =
   if 0 = Array.length a
      then a
   else
      let
	 fun x i = sub (a, i)
	 fun swap (i, j) =
	    let
	       val t = x i
	       val () = update (a, i, x j)
	       val () = update (a, j, t)
	    in
	       ()
	    end
	 val cutoff = 20
	 fun qsort (l: int, u: int): unit =
	    if Int.<= (u - l, cutoff)
	       then ()
	    else
	       let
		  val _ = swap (l, randInt (l, u))
		  val t = x l
		  (* Partition based on page 115. *)
		  fun loop (i, j) =
		     let
			fun loopUp i =
			   let
			      val i = i + 1
			   in
			      (* The sentinel guarantees that x i is OK. *)
			      if t <= x i
				 then i
			      else loopUp i
			   end
			val i = loopUp i
			fun loopDown j =
			   let
			      val j = j - 1
			   in
			      if x j <= t
				 then j
			      else loopDown j
			   end
			val j = loopDown j
		     in
			if j < i
			   then (i, j)
			else (swap (i, j); loop (i, j))
		     end
		  val (i, j) = loop (l, u + 1)
		  val () = swap (l, j)
		  val () = qsort (l, j - 1)
		  val () = qsort (i, u)
	       in
		  ()
	       end
	 (* Put a maximal element at the end to use as a sentinel. *)
	 val (m, _) =
	    Array.foldi
	    (a, (0, Array.sub (a, 0)), fn (i, xi, (m, xm)) =>
	     if xi <= xm
		then (m, xm)
	     else (i, xi))
	 val last = length a - 1
	 val () = swap (m, last)
	 val _ = qsort (0, last - 1)
	 val _ = InsertionSort.sort (a, op <=)
      in
	 a
      end

fun sortList (l, f) =
   Array.toList (sortArray (Array.fromList l, f))

fun sortVector (v, f) =
   Array.toVector (sortArray (Array.fromVector v, f))
   
end
