(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure String1 =
struct

open String0
	 
structure F = Fold (type 'a t = string
		    type 'a elt = char
		    val fold = fold)
open F
type t = string
	 
val last = String0.last
	 
val layout = Layout.str o escapeSML

(* This hash function is taken from pages 56-57 of
 * The Practice of Programming by Kernighan and Pike.
 *)
fun hash (s: t): Word.t =
   fold (s, 0w0, fn (c, h) => Word.fromChar c + Word.* (h, 0w31))
	 
fun dropl (s, p) =
   case peeki (s, fn (_, c) => not (p c)) of
      NONE => ""
    | SOME (i, _) => extract (s, i, NONE)

fun deleteSurroundingWhitespace (s: t): t =
   let
      val n = size s
      fun loop (i: int) =
	 if Int.>= (i, n)
	    then s
	 else
	    if Char.isSpace (sub (s, i))
	       then loop (i + 1)
	    else
	       let
		  fun loop (j: int) =
		     let
			val c = sub (s, j)
		     in
			if Int.<= (j, i)
			   then fromChar c
			else
			   if Char.isSpace c
			      then loop (j - 1)
			   else extract (s, i, SOME (j - i + 1))
		     end
	       in
		  loop (n - 1)
	       end
   in loop 0
   end

fun rev (s: t): t =
   let
      val n = size s
      val n1 = n - 1
   in
      CharVector.tabulate (n, fn i => sub (s, n1 - i))
   end

val fromListRev = rev o implode

end
