(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor EquivalenceRelation (S: EQUIVALENCE_RELATION_STRUCTS):>
   EQUIVALENCE_RELATION = 
struct

open S

(* An equivalence relation is represented as a list of pairs of equivalent
 * integers, except that reflexive pairs (e.g. (1, 1)) are not listed, and
 * of the pairs (a, b) (b, a) where a < b, only (a, b) is listed
 * The list of pairs is kept sorted in increasing lexicographic order.
 *)

datatype t = T of (int * int) list

fun layout (T ps) =
   List.layout (fn (x, y) => Layout.tuple [Int.layout x, Int.layout y]) ps

fun make (xs, equals) =
   let
      val xs = List.mapi (xs, fn ix => ix)
      val rec loop =
	 fn (xs, accum) =>
	 case xs of
	    [] => List.rev accum
	  | (i,x)::xs =>
	       loop (xs,
		    case List.keepAll (xs, fn (_,x') => equals (x,x')) of
		       [] => accum
		     | yes => List.fold (yes, accum, fn ((i',_),accum) =>
					(i,i')::accum))
   in T (loop (xs, []))
   end

fun universal n =
   let
      val rec loop =
	 fn [] => []
	  | x :: l => List.appendMap (l, fn y => (x, y), loop l)
   in T (loop (Int.map (0, n, fn i => i)))
   end

fun refine (T r, f) = T (List.keepAll (r, f))

fun areEquivalent (T r, x, y) =
   let val x' = Int.min (x,y)
      val y' = Int.max (x,y)
   in List.exists (r, fn (a,b) => a = x' andalso b = y')
   end

val equals = op =
   
fun classes (T pairs) =
   let val max = List.fold (pairs, ~1, fn ((_,y),m) =>
			   Int.max (y,m))
      val done = Array.new (max + 1, false)
      fun loop (pairs, i, class, classes) =
	 case pairs of
	    [] => class::classes
	  | (j,k)::pairs =>
	       if i = j 
		  then let val class = if Array.sub (done,k)
					  then class
				       else (Array.update (done,k,true) ;
					     k::class)
		       in loop (pairs, i, class, classes)
		       end
	       else
		  start ((j,k)::pairs,
			case class of
			   [_] => classes
			 | _ => class::classes)
      and start (pairs,classes) =
	 case pairs of
	    [] => classes
	  | (i,j)::pairs => if Array.sub (done,i)
			       then start (pairs,classes)
			    else (Array.update (done,j,true)
				  ; loop (pairs, i, [j,i], classes))
   in if max = ~1
	 then []
      else start (pairs, [])
   end

(* val classes = Trace.trace ("classes", layout,
 * 			  fn cs =>
 * 			  let open Layout
 * 			  in list (List.map (cs, fn c =>
 * 					   list (List.map (c,Int.layout))))
 * 			  end) classes
 *)
end

structure EquivalenceRelation = EquivalenceRelation ()
