(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure IEEEReal: IEEE_REAL =
   struct
      exception Unordered
      datatype real_order = LESS | EQUAL | GREATER | UNORDERED
      datatype nan_mode = QUIET | SIGNALLING

      datatype float_class =
	 NAN of nan_mode 
       | INF
       | ZERO
       | NORMAL
       | SUBNORMAL
	 
      datatype rounding_mode =
	 TO_NEAREST
       | TO_NEGINF
       | TO_POSINF
       | TO_ZERO

      val intToRounding_mode: int -> rounding_mode =
	 fn 0 => TO_NEAREST
	  | 1 => TO_NEGINF
	  | 2 => TO_POSINF
	  | 3 => TO_ZERO
	  | _ => raise Fail "IEEEReal.intToRounding_mode"

      val rounding_modeToInt: rounding_mode -> int =
	 fn TO_NEAREST => 0
	  | TO_NEGINF => 1
	  | TO_POSINF => 2
	  | TO_ZERO => 3

      structure Prim = Primitive.IEEEReal

      val setRoundingMode = Prim.setRoundingMode o rounding_modeToInt
      val getRoundingMode = intToRounding_mode o Prim.getRoundingMode
	       
      type decimal_approx = {kind: float_class,
			     sign: bool,
			     digits: int list,
			     exp: int}

      fun toString{kind, sign, digits, exp}: string =
	 let
	    fun digitStr() = implode(map StringCvt.digitToChar digits)
	    fun norm() =
	       let val num = "0." ^ digitStr()
	       in if exp = 0
		     then num
		  else concat[num, "E", Int.toString exp]
	       end
	    val num =
	       case kind of
		  ZERO => "0.0"
		| NORMAL => norm()
		| SUBNORMAL => norm()
		| INF => "inf"
		| NAN _ => concat["nan(", digitStr(), ")"]
	 in if sign
	       then "~" ^ num
	    else num
	 end
   end

