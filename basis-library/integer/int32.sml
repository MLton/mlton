(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Int32: INTEGER_EXTRA =
   struct
      structure Int = Primitive.Int
      open Int
	 
      val precision: int option = SOME 32

      val maxInt: int option = SOME 0x7fffffff
      val minInt: int option = SOME ~0x80000000
      local fun ident x = x
      in val toInt = ident
	 val fromInt = ident
      end
      (* These are overriden in patch.sml after int-inf.sml has been defined. *)
      val toLarge: int -> LargeInt.int = fn _ => raise Fail "toLarge"
      val fromLarge: LargeInt.int -> int = fn _ => raise Fail "fromLarge"

      val maxInt' = valOf maxInt
      val minInt' = valOf minInt

      val detectOverflow = Primitive.detectOverflow
	 
      fun quot (x, y) =
	 if y = 0
	    then raise Div
	 else if detectOverflow andalso x = minInt' andalso y = ~1
		 then raise Overflow
	      else Int.quot (x, y)
		 
      fun rem (x, y) =
	 if y = 0
	    then raise Div
	 else if x = minInt' andalso y = ~1
		 then 0
	      else Int.rem (x, y)
   
      fun x div y =
	 if x >= 0
	    then if y > 0
		    then Int.quot (x, y)
		 else if y < 0
			 then if x = 0
				 then 0
			      else Int.quot (x - 1, y) -? 1
		      else raise Div
	 else if y < 0
		 then if detectOverflow andalso x = minInt' andalso y = ~1
			 then raise Overflow
		      else Int.quot (x, y)
	      else if y > 0
		      then Int.quot (x + 1, y) -? 1
		   else raise Div

      fun x mod y =
	 if x >= 0
	    then if y > 0
		    then Int.rem (x, y)
		 else if y < 0
			 then if x = 0
				 then 0
			      else Int.rem (x - 1, y) +? (y + 1)
		      else raise Div
	 else if y < 0
		 then if x = minInt' andalso y = ~1
			 then 0
		      else Int.rem (x, y)
	      else if y > 0
		      then Int.rem (x + 1, y) +? (y - 1)
		   else raise Div

      val sign: int -> int =
	 fn 0 => 0
	  | i => if i < 0 then ~1 else 1
	       
      fun sameSign (x, y) = sign x = sign y

      fun abs (x: int) = if x < 0 then ~ x else x

      val {compare, min, max} = Util.makeCompare (op <)

      fun fmt radix (n: int): string =
	 let
	    val radix = fromInt (StringCvt.radixToInt radix)
	    fun loop (q, chars) =
	       let
		  val chars =
		     StringCvt.digitToChar (toInt (~? (rem (q, radix)))) :: chars
		  val q = quot (q, radix)
	       in if q = 0
		     then String0.implode (if n < 0 then #"~" :: chars
					   else chars)
		  else loop (q, chars)
	       end
	 in loop (if n < 0 then n else ~? n, [])
	 end
      
      val toString = fmt StringCvt.DEC
	 
      fun scan radix reader state =
	 let
	    (* Works with the negative of the number so that minInt can
	     * be scanned.
	     *)
	    val state = StringCvt.skipWS reader state
	    val charToDigit = fromInt (StringCvt.charToDigit radix)
	    val radixInt = fromInt (StringCvt.radixToInt radix)
	    fun finishNum (state, n) =
	       case reader state of
		  NONE => SOME (n, state)
		| SOME (c, state') =>
		     case charToDigit c of
			NONE => SOME (n, state)
		      | SOME n' => finishNum (state', n * radixInt - n')
	    fun num state =
	       case (reader state, radix) of
		  (NONE, _) => NONE
		| (SOME (#"0", state), StringCvt.HEX) =>
		     (case reader state of
			 NONE => SOME (0, state)
		       | SOME (c, state') =>
			    let
			       fun rest () =
				  case reader state' of
				     NONE => SOME (0, state)
				   | SOME (c, state') =>
					case charToDigit c of
					   NONE => SOME (0, state)
					 | SOME n => finishNum (state', ~? n)
			    in case c of
			       #"x" => rest ()
			     | #"X" => rest ()
			     | _ => (case charToDigit c of
					NONE => SOME (0, state)
				      | SOME n => finishNum (state', ~? n))
			    end)
		| (SOME (c, state), _) =>
		     (case charToDigit c of
			 NONE => NONE
		       | SOME n => finishNum (state, ~? n))
	    fun negate state =
	       case num state of
		  NONE => NONE
		| SOME (n, s) => SOME (~ n, s)
	 in case reader state of
	    NONE => NONE
	  | SOME (c, state') =>
	       case c of
		  #"~" => num state'
		| #"-" => num state'
		| #"+" => negate state'
		| _ => negate state
	 end
      
      val fromString = fromInt o (StringCvt.scanString (scan StringCvt.DEC))

      fun power {base, exp} =
	 if Primitive.safe andalso exp < 0
	    then raise Fail "Int.power"
	 else let
		 fun loop (exp, accum) =
		    if exp <= 0
		       then accum
		    else loop (exp - 1, base * accum)
	      in loop (exp, 1)
	      end
   end

structure Int = Int32
structure IntGlobal: INTEGER_GLOBAL = Int
open IntGlobal
structure Position = Int
