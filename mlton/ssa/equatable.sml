(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure Equatable: EQUATABLE =
struct

structure Set = DisjointSet

datatype 'a delay =
   Computed of 'a
 | Uncomputed of unit -> 'a

datatype 'a t = T of 'a delay Set.t

fun layout f (T s) =
   case Set.! s of
      Computed a => f a
    | Uncomputed _ => Layout.str "<uncomputed>"

fun delay f = T (Set.singleton (Uncomputed f))
	 
fun new a = T (Set.singleton (Computed a))

fun equals (T s, T s') = Set.equals (s, s')

fun value (T s) =
   case Set.! s of
      Computed a => a
    | Uncomputed f =>
	 let
	    val a = f ()
	    val () = Set.:= (s, Computed a)
	 in
	    a
	 end
	    
fun equate (T s, T s', combine) =
   if Set.equals (s, s')
      then ()
   else
      let
	 val d = Set.! s
	 val d' = Set.! s'
	 val () = Set.union (s, s')
      in
	 case (d, d') of
	    (Computed a, Computed a') =>
	       Set.:= (s, Computed (combine (a, a')))
	  | (Uncomputed _, _) => Set.:= (s, d')
	  | (_, Uncomputed _) => Set.:= (s, d)
      end

end
