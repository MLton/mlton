(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Instream0 =
struct

structure String = String0
structure Char = String.Char
structure I = Pervasive.TextIO
open I
   
type t = I.instream

val standard = stdIn
val close = closeIn
val inputChar = input1
val peekChar = lookahead   
val endOf = endOfStream

fun foldChars (ins, a, f) =
   let
      fun loop a =
	 case inputLine ins of
	    "" => a
	  | l => loop (String.fold (l, a, f))
   in
      loop a
   end
   
fun foldLines (ins, ac, f) =
   let
      fun loop ac =
	 case inputLine ins of
	    "" => ac
	  | l => loop (f (l, ac))
   in loop ac
   end

fun foreachLine (ins, f) = foldLines (ins, (), f o #1)

fun inputTo (i, p) =
   let
      fun loop accum =
	 case peekChar i of
	    NONE => accum
	  | SOME c => if p c then accum else (inputChar i; loop (c :: accum))
   in String.implode (rev (loop []))
   end

fun sameContents (in1, in2) =
   let
      fun loop () =
	 case (input1 in1, input1 in2) of
	    (NONE, NONE) => true
	  | (SOME c1, SOME c2) => Char.equals (c1, c2) andalso loop ()
	  | _ => false
   in loop ()
   end

fun inputToSpace i = inputTo (i, Char.isSpace)
fun inputToChar (i, c) = inputTo (i, fn c' => Char.equals (c, c'))
fun ignoreSpaces i = (inputTo (i,not o Char.isSpace); ())

fun inputNothing _ = ()

fun layout _ = Layout.str "<instream>"

(*val set = MLton.TextIO.setIn *)

end

structure In0 = Instream0
