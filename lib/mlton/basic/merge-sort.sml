(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure MergeSort: MERGE_SORT =
struct

type int = Int.t

(* This is a variant of mergesort that runs in O (n log n) time. *)
fun make (op <= : 'a * 'a -> bool) =
   let
      fun assert f = Assert.assert ("sort", f)
      fun isSorted l =
	 case l of
	    [] => true
	  | x :: l =>
	       let
		  fun loop (x, l) =
		     case l of
			[] => true
		      | x' :: l => x <= x' andalso loop (x', l)
	       in loop (x, l)
	       end
      fun merge (l1, l2) =
	 (assert (fn () => isSorted l1 andalso isSorted l2)
	  ; (case (l1, l2) of
		([], _) => l2
	      | (_, []) => l1
	      | (x1 :: l1', x2 :: l2') =>
		   if x1 <= x2
		      then x1 :: merge (l1', l2)
		   else x2 :: merge (l1, l2')))
      fun sort l =
	 let
	    val numBuckets = 25
	    val _ = assert (fn () => length l < Int.^ (2, numBuckets) - 1)
	    val a: 'a list array = Array.new (numBuckets, [])
	    fun invariant () =
	       assert (fn () => Array.foralli (a, fn (i, l) =>
					       case l of
						  [] => true
						| _ => (length l = Int.^ (2, i)
							andalso isSorted l)))
	    fun mergeIn (i: int, l: 'a list): unit =
	       (assert (fn () => length l = Int.^ (2, i))
		; (case Array.sub (a, i) of
		      [] => Array.update (a, i, l)
		    | l' => (Array.update (a, i, [])
			     ; mergeIn (i + 1, merge (l, l')))))
	    val _ = List.foreach (l, fn x => mergeIn (0, [x]))
	    val l = Array.fold (a, [], fn (l, l') =>
			       case l of
				  [] => l'
				| _ => merge (l, l'))
	    val _ = assert (fn () => isSorted l)
	 in l
	 end
   in {isSorted = isSorted,
       merge = merge,
       sort = sort}
   end

fun sort (l, le) = #sort (make le) l

fun merge (l, l', le) = #merge (make le) (l, l')
   
end
