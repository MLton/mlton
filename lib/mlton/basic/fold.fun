(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Fold (S: FOLD_STRUCTS): FOLD = 
struct

structure Int = Pervasive.Int
type int = Int.int
   
open S

fun foldi (l, b, f) =
   #1 (fold (l, (b, 0: int), fn (x, (b, i)) => (f (i, x, b), i + 1)))

fun foreachi (l, f) = foldi (l, (), fn (i, x, ()) => f (i, x))

fun foreach (l, f) = fold (l, (), f o #1)

fun last l =
   case fold (l, NONE, SOME o #1) of
      NONE => Error.bug "last"
    | SOME x => x
   
fun length l = fold (l, 0: int, fn (_, n) => n + 1)

fun mapi (l, f) = rev (foldi (l, [], fn (i, x, l) => f (i, x) :: l))

fun map (l, f) = mapi (l, f o #2)
   
fun layout f l = Layout.list (map (l, f))

fun keepAllMap (l, f) =
   rev (fold (l, [], fn (x, l) =>
	      case f x of
		 NONE => l
	       | SOME y => y :: l))

fun keepAll (l, f) = keepAllMap (l, fn x => if f x then SOME x else NONE)
			       
val subset = keepAll

fun removeAll (l, f) = subset (l, not o f)

end

