structure Real64: REAL =
   struct
      structure Prim = Primitive.Real
      local
	 open IEEEReal
      in
	 datatype z = datatype float_class
	 datatype z = datatype rounding_mode
      end
      infix 4 == != ?=
      type real = real

      local
	 open Prim
      in
	 val *+ = *+
	 val *- = *-
	 val abs = abs
	 val copySign = copySign
	 val fromInt = fromInt
	 val isFinite = isFinite
	 val isNan = isNan
	 val isNormal = isNormal
	 val maxFinite = maxFinite
	 val minNormalPos = minNormalPos
	 val minPos = minPos
	 val nextAfter = nextAfter
	 val op * = op *
	 val op + = op +
	 val op - = op -
	 val op / = op /
	 val op / = op /
	 val op < = op <
	 val op <= = op <=
	 val op == = op ==
	 val op > = op >
	 val op >= = op >=
	 val op ?= = op ?=
	 val signBit = signBit
	 val ~ = ~
      end

      val op ?= =
	 if Primitive.MLton.native
	    then op ?=
	 else fn (r, r') => isNan r orelse isNan r' orelse r == r'
	 
      val radix: int = 2

      val precision: int = 52

      val posInf = 1.0 / 0.0
      val negInf = ~1.0 / 0.0

      val nan = posInf + negInf
	 
      structure Math =
	 struct
	    open Prim.Math

	    structure MLton = Primitive.MLton
	    structure Platform = MLton.Platform
	    (* Patches for Cygwin and SunOS, whose math libraries do not handle
	     * out-of-range args.
	     *)
	    val (acos, asin, ln, log10) =
	       if not MLton.native
		  andalso (case Platform.os of
			      Platform.Cygwin => true
			    | Platform.SunOS => true
			    | _ => false)
		  then
		     let
			fun patch f x =
			   if x < ~1.0 orelse x > 1.0
			      then nan
			   else f x
			val acos = patch acos
			val asin = patch asin
			fun patch f x = if x < 0.0 then nan else f x
			val ln = patch ln
			val log10 = patch log10
		     in
			(acos, asin, ln, log10)
		     end
	       else (acos, asin, ln, log10)
	 end

      val op != = not o op ==

      fun min (x, y) = if x < y orelse isNan y then x else y

      fun max (x, y) = if x > y orelse isNan y then x else y

      fun sign (x: real): int =
	 if x > 0.0 then 1
         else if x < 0.0 then ~1
	 else if isNan x then raise Domain
         else 0

      fun sameSign (x, y) = Prim.signBit x = Prim.signBit y

      local
	 datatype z = datatype General.order
      in
	 fun compare (x, y) =
	    if x < y then LESS
	    else if x > y then GREATER
            else if x == y then EQUAL
            else raise IEEEReal.Unordered
      end

      local
	 datatype z = datatype IEEEReal.real_order
      in
	 fun compareReal (x, y) = 
	    if x < y then LESS
	    else if x > y then GREATER
            else if x == y then EQUAL 
            else UNORDERED
      end
   
      fun unordered (x, y) = isNan x orelse isNan y

      (* See runtime/basis/Real.c for the integers returned by class. *)
      fun class x =
	 case Prim.class x of
	    0 => NAN (* QUIET *)
	  | 1 => NAN (* SIGNALLING *)
	  | 2 => INF
	  | 3 => ZERO
	  | 4 => NORMAL
	  | 5 => SUBNORMAL
	  | _ => raise Fail "Primitive.Real.class returned bogus integer"

      val toManExp =
	 let
	    val r: int ref = ref 0
	 in
	    fn x => if x == 0.0
		       then {exp = 0, man = 0.0}
		    else
		       let
			  val man = Prim.frexp (x, r)
		       in
			  {man = man * 2.0, exp = Int.- (!r, 1)}
		       end
	 end

      fun fromManExp {man, exp} = Prim.ldexp (man, exp)

      local
	 val int = ref 0.0
      in
	 fun split x =
	    let
	       val frac = Prim.modf (x, int)
	    in
	       {frac = frac,
		whole = ! int}
	    end
      end

      val realMod = #frac o split
	 
      fun checkFloat x =
	 case class x of
	    INF => raise Overflow
	  | NAN => raise Div
	  | _ => x

      fun withRoundingMode (m, th) =
	 let
	    val m' = IEEEReal.getRoundingMode ()
	    val _ = IEEEReal.setRoundingMode m
	    val res = th ()
	    val _ = IEEEReal.setRoundingMode m'
	 in
	    res
	 end

      val maxInt = fromInt Int.maxInt'
      val minInt = fromInt Int.minInt'

      fun toInt mode x =
	 let
	    fun doit () = withRoundingMode (mode, fn () =>
					    Prim.toInt (Prim.round x))
	 in
	    case class x of
	       NAN => raise Domain
	     | INF => raise Overflow
	     | ZERO => 0
	     | NORMAL =>
		  if minInt <= x
		     then if x <= maxInt
			     then doit ()
			  else if x < maxInt + 1.0
				  then (case mode of
					   TO_NEGINF => Int.maxInt'
					 | TO_POSINF => raise Overflow
					 | TO_ZERO => Int.maxInt'
					 | TO_NEAREST =>
					      (* Depends on maxInt being odd. *)
					      if x - maxInt >= 0.5
						 then raise Overflow
					      else Int.maxInt')
			       else raise Overflow
		  else if x > minInt - 1.0
			  then (case mode of
				   TO_NEGINF => raise Overflow
				 | TO_POSINF => Int.minInt'
				 | TO_ZERO => Int.minInt'
				 | TO_NEAREST =>
				      (* Depends on minInt being even. *)
				      if x - minInt < ~0.5
					 then raise Overflow
				      else Int.minInt')
		       else raise Overflow
           | SUBNORMAL => doit ()
	 end

      fun toLarge x = x
      fun fromLarge _ x = x
      
      val floor = toInt TO_NEGINF
      val ceil = toInt TO_POSINF
      val trunc = toInt TO_ZERO
      val round = toInt TO_NEAREST

      local
	 fun round mode x =
	    case class x of
	       NAN => x
	     | INF => x
	     | _ => withRoundingMode (mode, fn () => Prim.round x)
      in
	 val realFloor = round TO_NEGINF
	 val realCeil = round TO_POSINF
	 val realTrunc = round TO_ZERO
      end

      fun rem (x, y) =
	 case class x of
	    INF => nan
	  | NAN => nan
	  | ZERO => 0.0
	  | _ =>
	       case class y of
		  INF => x
		| NAN => nan
		| ZERO => nan
		| _ => x - realTrunc (x/y) * y

      (* fromDecimal, scan, fromString: decimal -> binary conversions *)
      exception Bad
      fun fromDecimal ({class, digits, exp, sign}: IEEEReal.decimal_approx) =
	 let
	    fun doit () =
	       let
		  val exp =
		     if Int.< (exp, 0)
			then concat ["-", Int.toString (Int.~ exp)]
		     else Int.toString exp
		  val x =
		     concat ["0.",
			     implode (List.map
				      (fn d =>
				       if Int.< (d, 0) orelse Int.> (d, 9)
					  then raise Bad
				       else Char.chr (Int.+ (d, Char.ord #"0")))
				      digits),
			     "E", exp, "\000"]
		  val x = Prim.strtod x
	       in
		  if sign
		     then ~ x
		  else x
	       end
	 in
	    SOME (case class of
		     INF => if sign then negInf else posInf
		   | NAN => nan
		   | NORMAL => doit ()
		   | SUBNORMAL => doit ()
		   | ZERO => 0.0)
	    handle Bad => NONE
	 end

      fun scan reader state =
	 case IEEEReal.scan reader state of
	    NONE => NONE
	  | SOME (da, state) => SOME (valOf (fromDecimal da), state)

      val fromString = StringCvt.scanString scan

      (* toDecimal, fmt, toString: binary -> decimal conversions. *)
      datatype mode = Fix | Gen | Sci
      local
	 val decpt: int ref = ref 0
      in
	 fun gdtoa (x: real, mode: mode, ndig: int) =
	    let
	       val mode =
		  case mode of
		     Fix => 3
		   | Gen => 0
		   | Sci => 2
	       val cs = Prim.gdtoa (x, mode, ndig, decpt)
	    in
	       (cs, !decpt)
	    end
      end
   
      fun toDecimal (x: real): IEEEReal.decimal_approx =
	 case class x of
	    NAN => {class = NAN,
		    digits = [],
		    exp = 0,
		    sign = false}
	  | INF => {class = INF,
		    digits = [],
		    exp = 0,
		    sign = x < 0.0}
	  | ZERO => {class = ZERO,
		     digits = [],
		     exp = 0,
		     sign = false}
	  | c => 
	       let
		  val (cs, decpt) = gdtoa (x, Gen, 0)
		  fun loop (i, ac) =
		     if Int.< (i, 0)
			then ac
		     else loop (Int.- (i, 1),
				(Int.- (Char.ord (C.CS.sub (cs, i)),
					Char.ord #"0"))
				:: ac)
		  val digits = loop (Int.- (C.CS.length cs, 1), [])
		  val exp = decpt
	       in
		  {class = NORMAL,
		   digits = digits,
		   exp = exp,
		   sign = x < 0.0}
	       end

      datatype realfmt = datatype StringCvt.realfmt

      fun add1 n = Int.+ (n, 1)
	 
      local
	 fun fix (sign: string, cs: C.CS.t, decpt: int, ndig: int): string =
	    let
	       val length = C.CS.length cs
	    in
	       if Int.< (decpt, 0)
		  then
		     concat [sign,
			     "0.",
			     String.new (Int.~ decpt, #"0"),
			     C.CS.toString cs,
			     String.new (Int.+ (Int.- (ndig, length),
						decpt),
					 #"0")]
	       else
		  let 
		     val whole =
			if decpt = 0
			   then "0"
			else
			   String.tabulate (decpt, fn i =>
					    if Int.< (i, length)
					       then C.CS.sub (cs, i)
					    else #"0")
		  in
		     if 0 = ndig
			then concat [sign, whole]
		     else
			let
			   val frac =
			      String.tabulate
			      (ndig, fn i =>
			       let
				  val j = Int.+ (i, decpt)
			       in
				  if Int.< (j, length)
				     then C.CS.sub (cs, j)
				  else #"0"
			       end)
			in
			   concat [sign, whole, ".", frac]
			end
		  end
	    end
	 fun sci (sign: string, cs: C.CS.t, decpt: int, ndig: int): string =
	    let
	       val length = C.CS.length cs
	       val whole = String.tabulate (1, fn _ => C.CS.sub (cs, 0))
	       val frac =
		  if 0 = ndig
		     then ""
		  else concat [".",
			       String.tabulate
			       (ndig, fn i =>
				let
				   val j = Int.+ (i, 1)
				in
				   if Int.< (j, length)
				      then C.CS.sub (cs, j)
				   else #"0"
				end)]
	       val exp = Int.- (decpt, 1)
	       val exp =
		  let
		     val (exp, sign) =
			if Int.< (exp, 0)
			   then (Int.~ exp, "~")
			else (exp, "")
		  in
		     concat [sign, Int.toString exp]
		  end
	    in
	       concat [sign, whole, frac, "E", exp]
	    end
			
      in
	 fun fmt spec =
	    let
	       val doit =
		  case spec of
		     EXACT => IEEEReal.toString o toDecimal
		   | FIX opt =>
			let
			   val n =
			      case opt of
				 NONE => 6
			       | SOME n =>
				    if Primitive.safe andalso Int.< (n, 0)
				       then raise Size
				    else n
			in
			   fn x =>
			   let
			      val sign = if x < 0.0 then "~" else ""
			      val (cs, decpt) = gdtoa (x, Fix, n)
			   in
			      fix (sign, cs, decpt, n)
			   end
			end
		   | GEN opt =>
			let
			   val n =
			      case opt of
				 NONE => 12
			       | SOME n =>
				    if Primitive.safe andalso Int.< (n, 1)
				       then raise Size
				    else n
			in
			   fn x =>
			   let
			      val sign = if x < 0.0 then "~" else ""
			      val (cs, decpt) = gdtoa (x, Sci, n)
			      val length = C.CS.length cs
			   in
			      if Int.<= (decpt, ~4)
				 orelse Int.> (decpt, Int.+ (5, length))
				 then sci (sign, cs, decpt, Int.- (length, 1))
			      else fix (sign, cs, decpt,
					if Int.< (length, decpt)
					   then 0
					else Int.- (length, decpt))
			   end
			end
		   | SCI opt =>
			let
			   val n =
			      case opt of
				 NONE => 6
			       | SOME n =>
				    if Primitive.safe andalso Int.< (n, 0)
				       then raise Size
				    else n
			in
			   fn x =>
			   let
			      val sign = if x < 0.0 then "~" else ""
			      val (cs, decpt) = gdtoa (x, Sci, add1 n)
			   in
			      sci (sign, cs, decpt, n)
			   end
			end
	    in
	       fn x =>
	       case class x of
		  NAN => "nan"
		| INF => if x > 0.0 then "inf" else "~inf"
		| _ => doit x
	    end
      end
   
      val toString = fmt (StringCvt.GEN NONE)

      local
	 fun negateMode m =
	    case m of
	       TO_NEAREST => TO_NEAREST
	     | TO_NEGINF => TO_POSINF
	     | TO_POSINF => TO_NEGINF
	     | TO_ZERO => TO_ZERO

	 val m: int = 52 (* The number of mantissa bits in 64 bit IEEE 854. *)
	 val half = Int.quot (m, 2)
	 val two = IntInf.fromInt 2
	 val twoPowHalf = IntInf.pow (two, half)
      in
	 fun fromLargeInt (i: IntInf.int): real =
	    let
	       fun pos (i: IntInf.int, mode): real = 
		  case SOME (IntInf.log2 i) handle Overflow => NONE of
		     NONE => posInf
		   | SOME exp =>
			if Int.< (exp, Int.- (valOf Int.precision, 1))
			   then fromInt (IntInf.toInt i)
			else if Int.>= (exp, 1024)
		           then posInf
			else
			   let
			      val shift = Int.- (exp, m)
			      val (man: IntInf.int, extra: IntInf.int) =
				 if Int.>= (shift, 0)
				    then
				       let
					  val (q, r) =
					     IntInf.quotRem
					     (i, IntInf.pow (two, shift))
					  val extra =
					     case mode of
						TO_NEAREST =>
						   if IntInf.> (r, 0)
						      andalso IntInf.log2 r =
						      Int.- (shift, 1)
						      then 1
						   else 0
					      | TO_NEGINF => 0
					      | TO_POSINF =>
						   if IntInf.> (r, 0)
						      then 1
						   else 0
					      | TO_ZERO => 0
				       in
					  (q, extra)
				       end
				 else
				    (IntInf.* (i, IntInf.pow (two, Int.~ shift)),
				     0)
			      (* 2^m <= man < 2^(m+1) *)
			      val (q, r) = IntInf.quotRem (man, twoPowHalf)
			      fun conv (man, exp) =
				 fromManExp {man = fromInt (IntInf.toInt man),
					     exp = exp}
			   in
			      conv (q, Int.+ (half, shift))
			      + conv (IntInf.+ (r, extra), shift)
			   end
	       val mode = IEEEReal.getRoundingMode ()
	    in
	       case IntInf.compare (i, IntInf.fromInt 0) of
		  General.LESS => ~ (pos (IntInf.~ i, negateMode mode))
		| General.EQUAL => 0.0
		| General.GREATER => pos (i, mode)
	    end

	 val toLargeInt: IEEEReal.rounding_mode -> real -> IntInf.int =
	    fn mode => fn x =>
 	    (IntInf.fromInt (toInt mode x)
 	     handle Overflow =>
	     case class x of
		INF => raise Overflow
	      | _ => 
		   let
		      fun pos (x, mode) =
			 let 
			    val {frac, whole} = split x
			    val extra =
			       if mode = TO_NEAREST
				  andalso 0.5 == frac
				  then
				     if 0.5 == realMod (whole / 2.0)
					then 1
				     else 0
			       else IntInf.fromInt (toInt mode frac)
			    val {man, exp} = toManExp whole
			    (* 1 <= man < 2 *)
			    val man = fromManExp {man = man, exp = half}
			    (* 2^half <= man < 2^(half+1) *)
			    val {frac = lower, whole = upper} = split man
			    val upper = IntInf.* (IntInf.fromInt (floor upper),
						  twoPowHalf)
			    (* 2^m <= upper < 2^(m+1) *)
			    val {whole = lower, ...} =
			       split (fromManExp {man = lower, exp = half})
			    (* 0 <= lower < 2^half *)
			    val lower = IntInf.fromInt (floor lower)
			    val int = IntInf.+ (upper, lower)
			    (* 2^m <= int < 2^(m+1) *)
			    val shift = Int.- (exp, m)
			    val int =
			       if Int.>= (shift, 0)
				  then IntInf.* (int, IntInf.pow (2, shift))
			       else IntInf.quot (int,
						 IntInf.pow (2, Int.~ shift))
			 in
			    IntInf.+ (int, extra)
			 end
		   in
		      if x > 0.0
			 then pos (x, mode)
		      else IntInf.~ (pos (~ x, negateMode mode))
		   end)
      end
  end

structure Real = Real64   
structure LargeReal = Real64
structure RealGlobal: REAL_GLOBAL = Real
open RealGlobal
val real = Real.fromInt
