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

fun peekGen (l: 'a t, no: unit -> 'b, f: int * 'a elt -> 'b option): 'b =
   DynamicWind.withEscape
   (fn escape => (foreachi (l, fn (i, x) =>
			    case f (i, x) of
			       NONE => ()
			     | SOME yes => escape yes)
		  ; no ()))

fun peekMapi (l: 'a t, f: 'a elt -> 'b option): (int * 'b) option =
   peekGen (l,
	    fn () => NONE,
	    fn (i, a) => Option.map (f a, fn b => SOME (i, b)))

fun peekMap (l, f) = Option.map (peekMapi (l, f), #2)

fun peeki (l, f) =
   peekGen (l, fn () => NONE,
	    fn (i, x) => if f (i, x)
			    then SOME (SOME (i, x))
			 else NONE)

fun isEmpty l = peekGen (l, fn () => true, fn _ => SOME false)

fun nth (l, n) = peekGen (l,
			fn () => Error.bug "nth",
			fn (i, x) => if i = n then SOME x else NONE)

fun first l = peekGen (l, fn () => Error.bug "first", SOME o #2)

   
fun peek (l, f) = peekGen (l, fn () => NONE,
			 fn (_, x) => if f x then SOME (SOME x) else NONE)

fun index (l, f) = peekGen (l, fn () => NONE,
			  fn (i, x) => if f x then SOME (SOME i) else NONE)

fun existsi (l, f) = peekGen (l,
			    fn () => false,
			    fn (i, x) => if f (i, x) then SOME true else NONE)

fun exists (l, f) = existsi (l, fn (_, x) => f x)

fun foralli (l, f) = not (existsi (l, not o f))

fun forall (l, f) = foralli (l, f o #2)

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

fun lookup (l, f) =
   case peek (l, f) of
      NONE => Error.bug "lookup"
    | SOME x => x

end

