(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Word (W: sig
		   include PRE_WORD
		   val zero: word
		end): WORD_EXTRA =
struct

open W
   
(* These are overriden in patch.sml after int-inf.sml has been defined. *)
fun toLargeInt _ = raise Fail "toLargeInt"
fun toLargeIntX _ = raise Fail "toLargeIntX"
fun fromLargeInt _ = raise Fail "fromLargeInt"

local
   fun make f (w, w') =
      if Primitive.safe andalso w' = zero
	 then raise Div
      else f (w, w')
in val op div = make (op div)
   val op mod = make (op mod)
end

fun fmt radix (w: word): string =
   let val radix = fromInt (StringCvt.radixToInt radix)
      fun loop (q, chars) =
	 let val chars = StringCvt.digitToChar (toInt (q mod radix)) :: chars
	    val q = q div radix
	 in if q = zero
	       then String0.implode chars
	    else loop (q, chars)
	 end
   in loop (w, [])
   end

val toString = fmt StringCvt.HEX

fun scan radix reader state =
   let
      val state = StringCvt.skipWS reader state
      val charToDigit = StringCvt.charToDigit radix
      val radixWord = fromInt (StringCvt.radixToInt radix)
      fun finishNum (state, n) =
	 case reader state of
	    NONE => SOME (n, state)
	  | SOME (c, state') =>
	       case charToDigit c of
		  NONE => SOME (n, state)
		| SOME n' =>
		     let val n'' = n * radixWord
		     in if n'' div radixWord = n
			   then let val n' = fromInt n'
				   val n''' = n'' + n'
				in if n''' >= n''
				      then finishNum (state', n''')
				   else raise Overflow
				end
			else raise Overflow
		     end
      fun num state = finishNum (state, zero)
   in
      case reader state of
	 NONE => NONE
       | SOME (c, state) =>
	    case c of
	       #"0" =>
	       (case reader state of
		   NONE => SOME (zero, state)
		 | SOME (c, state') =>
		      case c of
			 #"w" => (case radix of
				     StringCvt.HEX =>
					(case reader state' of
					    NONE =>
					       (* the #"w" was not followed by
						* an #"X" or #"x", therefore we
						* return 0 *)
					       SOME (zero, state)
					  | SOME (c, state) =>
					       (case c of
						   #"x" => num state
						 | #"X" => num state
						 | _ =>
						 (* the #"w" was not followed by
						  * an #"X" or #"x", therefore we
						  * return 0 *)
						      SOME (zero, state)))
				   | _ => num state')
		       | #"x" => (case radix of
				     StringCvt.HEX => num state'
				   | _ => NONE)
		       | #"X" => (case radix of
				     StringCvt.HEX => num state'
				   | _ => NONE)
		       | _ => num state)
	     | _ => (case charToDigit c of
			NONE => NONE
		      | SOME n => finishNum (state, fromInt n))
   end

val fromString = StringCvt.scanString (scan StringCvt.HEX)

end
