(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Word (W: PRE_WORD_EXTRA): WORD_EXTRA =
struct

open W
structure PW = Primitive.Word

val detectOverflow = Primitive.detectOverflow

(* These are overriden in patch.sml after int-inf.sml has been defined. *)
val toLargeInt: word -> LargeInt.int = fn _ => raise Fail "toLargeInt"
val toLargeIntX: word -> LargeInt.int = fn _ => raise Fail "toLargeIntX"
val fromLargeInt: LargeInt.int -> word = fn _ => raise Fail "fromLargeInt"

val wordSizeWord: Word.word = PW.fromInt wordSize
val wordSizeMinusOneWord: Word.word = PW.fromInt (Int.-? (wordSize, 1))
val zero: word = fromInt 0

val toLargeWord = toLarge
val toLargeWordX = toLargeX
val fromLargeWord = fromLarge

fun toInt w =
   if detectOverflow
      andalso Int.>= (wordSize, Int.precision')
      andalso w > fromInt Int.maxInt'
      then raise Overflow
   else W.toInt w
		      
fun toIntX w =
  if detectOverflow
     andalso Int.> (wordSize, Int.precision')
     andalso fromInt Int.maxInt' < w
     andalso w < fromInt Int.minInt'
     then raise Overflow
  else W.toIntX w

local
   fun make f (w, w') =
      if Primitive.safe andalso w' = zero
	 then raise Div
      else f (w, w')
in
   val op div = make (op div)
   val op mod = make (op mod)
end

fun << (i, n) 
  = if PW.>=(n ,wordSizeWord)
      then zero
      else W.<<(i, n)

fun >> (i, n) 
  = if PW.>=(n, wordSizeWord)
      then zero
      else W.>>(i, n)

fun ~>> (i, n) 
  = if PW.<(n, wordSizeWord)
      then W.~>>(i, n)
      else W.~>>(i, wordSizeMinusOneWord)

val {compare, min, max} = Util.makeCompare(op <)

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

structure Word8 = Word (Primitive.Word8)
structure Word16 = Word (Primitive.Word16)
structure Word32 = Word (Primitive.Word32)
structure Word64 = Word (Primitive.Word64)
structure Word = Word32
structure WordGlobal: WORD_GLOBAL = Word
open WordGlobal
