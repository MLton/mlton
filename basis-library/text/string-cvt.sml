(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure StringCvt: STRING_CVT_EXTRA =
   struct
      open Reader
	 
      datatype radix = BIN | OCT | DEC | HEX

      val radixToInt: radix -> int =
	 fn BIN => 2
	  | OCT => 8
	  | DEC => 10
	  | HEX => 16

      datatype realfmt =
	 SCI of int option 
       | FIX of int option 
       | GEN of int option 
       | EXACT
	 
      type ('a, 'b) reader = 'b -> ('a * 'b) option

      open Primitive.Int

      structure Char = Char0
      structure String = String0

      local
	 fun pad f c i s =
	    let val n = String.size s
	    in if n >= i then s
	       else f (s, String0.new (i -? n, c))
	    end
      in
	 val padLeft = pad (fn (s, pad) => String.^ (pad, s))
	 val padRight = pad String.^
      end

      fun splitl p f src =
	 let fun done chars = String0.implode (rev chars)
	    fun loop (src, chars) =
	       case f src of
		  NONE => (done chars, src)
		| SOME (c, src') =>
		     if p c
			then loop (src', c :: chars)
		     else (done chars, src)
	 in loop (src, [])
	 end

      fun takel p f s = #1 (splitl p f s)
      fun dropl p f s = #2 (splitl p f s)

      fun skipWS x = dropl Char.isSpace x

      type cs = int

      fun stringReader (s: string): (char, cs) reader =
	 fn i => if i >= String.size s
		    then NONE
		 else SOME (String.sub (s, i), i + 1)
		    
      fun 'a scanString (f: ((char, cs) reader -> ('a, cs) reader)) (s: string)
	: 'a option =
	 case f (stringReader s) 0 of
	    NONE => NONE
	  | SOME (a, _) => SOME a

      local
	 fun range (add: int, cmin: char, cmax: char): char -> int option =
	    let val min = Char.ord cmin
	    in fn c => if Char.<= (cmin, c) andalso Char.<= (c, cmax)
			  then SOME (add +? Char.ord c -? min)
		       else NONE
	    end

	 fun 'a combine (ds: (char -> 'a option) list): char -> 'a option =
	    Char.memoize
	    (fn c =>
	     let
		val rec loop =
		   fn [] => NONE
		    | d :: ds =>
			 case d c of
			    NONE => loop ds
			  | z => z
	     in loop ds
	     end)
	    
	 val bin = Char.memoize (range (0, #"0", #"1"))
	 val oct = Char.memoize (range (0, #"0", #"7"))
	 val dec = Char.memoize (range (0, #"0", #"9"))
	 val hex = combine [range (0, #"0", #"9"),
			   range (10, #"a", #"f"),
			   range (10, #"A", #"F")]
      in
	 fun charToDigit (radix: radix): char -> int option =
	    case radix of
	       BIN => bin
	     | OCT => oct
	     | DEC => dec
	     | HEX => hex
      end

      fun charsToInt (radix: radix): char list -> int option =
	 let
	    val f = charToDigit radix
	    val r = radixToInt radix
	    fun loop (cs, accum) =
	       case cs of
		  [] => SOME accum
		| c :: cs =>
		     case f c of
			NONE => NONE
		      | SOME d => loop (cs, accum * r + d)
	 in fn cs => loop (cs, 0)
	 end

      fun digits (radix, max, accum) reader state =
	 let
	    val r = radixToInt radix
	    fun loop (max, accum, state) =
	       let fun done () = SOME (accum, state)
	       in if max <= 0
		     then done ()
		  else
		     case reader state of
			NONE => done ()
		      | SOME (c, state) =>
			   case charToDigit radix c of
			      NONE => done ()
			    | SOME n => loop (max - 1, n + accum * r, state)
	       end
	 in loop (max, accum, state)
	 end

      fun digitsPlus (radix, max) reader state =
	 let
	    val r = radixToInt radix
	 in
	    case reader state of
	       NONE => NONE
	     | SOME (c, state) =>
		  case charToDigit radix c of
		     NONE => NONE
		   | SOME n => digits (radix, max -? 1, n) reader state
	 end

      fun digitsExact (radix, num) reader state =
	 let val r = radixToInt radix
	    fun loop (num, accum, state) =
	       if num <= 0
		  then SOME (accum, state)
	       else
		  case reader state of
		     NONE => NONE
		   | SOME (c, state) =>
			case charToDigit radix c of
			   NONE => NONE
			 | SOME n => loop (num - 1, n + accum * r, state)
	 in loop (num, 0, state)
	 end

      fun digits radix reader state =
	 let val r = radixToInt radix
	    fun loop (accum, state) =
	       case reader state of
		  NONE => SOME (accum, state)
		| SOME (c, state') =>
		     case charToDigit radix c of
			NONE => SOME (accum, state)
		      | SOME n => loop (n + accum * r, state')
	 in case reader state of
	    NONE => NONE
	  | SOME (c, state) =>
	       case charToDigit radix c of
		  NONE => NONE
		| SOME n => loop (n, state)
	 end

      fun digitToChar (n: int): char = String.sub ("0123456789ABCDEF", n)
   end
