(* scan is from ML Kit Version 3 basislib/real.sml *)

(* Some of this code was taken from SML/NJ real64.sml *)
(* real64.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Real64: REAL =
   struct
      structure Real = Primitive.Real
      open Real IEEEReal
      infix 4 == != ?=
      type real = real
 
      val radix: int = 2

      val precision: int = 52

      val posInf = 1.0 / 0.0
      val negInf = ~1.0 / 0.0

      val nan = posInf + negInf
	 
      structure Math =
	 struct
	    open Math

	    structure MLton = Primitive.MLton
	    (* Patches for Cygwin newlib, which does not handle out of range
	     * args.
	     *)
	    val (acos, asin, ln, log10) =
	       if not MLton.native andalso MLton.hostType = MLton.Cygwin
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
	 if isNan x then raise Domain
	 else if x > 0.0 then 1
	      else if x < 0.0 then ~1
		   else 0

      fun sameSign (x, y) = signBit x = signBit y

      fun compare (x, y) =
	 if x<y then General.LESS
	 else if x>y then General.GREATER
	      else if x == y then General.EQUAL 
		   else raise Unordered

      fun compareReal (x, y) = 
	 if x < y then LESS
	 else if x > y then GREATER
	      else if x == y then EQUAL 
		   else UNORDERED

      fun unordered (x, y) = isNan x orelse isNan y

      (* See runtime/basis/Real.c for the integers returned by class. *)
      fun class x =
	 case Real.class x of
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
			  val man = frexp (x, r)
		       in
			  {man = man * 2.0, exp = Int.- (!r, 1)}
		       end
	 end

      fun fromManExp {man, exp} = ldexp (man, exp)

      local
	 val int = ref 0.0
      in
	 fun split x =
	    let
	       val frac = modf (x, int)
	    in
	       {frac = frac,
		whole = ! int}
	    end
      end

      val realMod = #frac o split
	 
      fun rem (x, y) = y * #frac (split (x/y))
	 
      fun checkFloat x =
	 if x > negInf andalso x < posInf then x
	 else if isNan x then raise Div
	      else raise Overflow

      fun withRoundingMode (m, th) =
	 let
	    val m' = getRoundingMode ()
	    val _ = setRoundingMode m
	    val res = th ()
	    val _ = setRoundingMode m'
	 in
	    res
	 end

      val maxInt = fromInt Int.maxInt'
      val minInt = fromInt Int.minInt'

      fun toInt mode x =
	 let
	    fun doit () = withRoundingMode (mode, fn () =>
					    Real.toInt (Real.round x))
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
	     | _ => withRoundingMode (mode, fn () => Real.round x)
      in
	 val realFloor = round TO_NEGINF
	 val realCeil = round TO_POSINF
	 val realTrunc = round TO_ZERO
      end

      datatype realfmt = datatype StringCvt.realfmt

      local
	 fun makeBuffer n = Primitive.Array.array n
	 (* Large enough for most cases *)
	 val normalSize: int = 500
	 val buffer = makeBuffer normalSize
	 val sciExtra: int = 10
	 val fixExtra: int = 400
	 val genExtra: int = 10
      in
	 fun fmt spec =
	    let
	       val (formatString, bufSize) =
		  case spec of
		     SCI opt =>
			let
			   val n =
			      case opt of
				 NONE => 6
			       | SOME n =>
				    if Primitive.safe andalso Int.< (n, 0)
				       then raise Size
				    else n
			in (concat ["%.", Int.toString n, "e"],
			    Int.+ (n, sciExtra))
			end
		   | FIX opt =>
			let
			   val n =
			      case opt of
				 NONE => 6
			       | SOME n =>
				    if Primitive.safe andalso Int.< (n, 0)
				       then raise Size
				    else n
			in (concat ["%.", Int.toString n, "f"],
			    Int.+ (n, fixExtra))
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
			in (concat ["%.", Int.toString n, "g"],
			    Int.+ (n, genExtra))
			end
		   | EXACT => raise Fail "Real.fmt EXACT unimplemented"
	    in fn x =>
	       case class x of
		  NAN => "nan"
		| INF => if x > 0.0 then "inf" else "~inf"
		| ZERO => "0.0"
		| _ => 
		     let
			val buffer =
			   if Int.> (bufSize, normalSize)
			      then makeBuffer bufSize
			   else buffer
			val len =
			   Primitive.Stdio.sprintf
			   (buffer, String.nullTerm formatString, x)
			val res = 
			   String.translate
			   (fn #"-" => "~" | c => str c)
			   (Array.extract (buffer, 0, SOME len))
		     in res
		     end
	    end
      end
   
      val toString = fmt (StringCvt.GEN NONE)

      (* Copied from MLKitV3 basislib/real.sml *)
      val real = fromInt
      fun scan getc source = 
	 let fun decval c = Int.- (Char.ord c, 48)
	    fun pow10 0 = 1.0
	      | pow10 n = 
		if Int.mod (n, 2) = 0 then 
		   let val x = pow10 (Int.div (n, 2)) in x * x end
		else 10.0 * pow10 (Int.- (n, 1))
	    fun pointsym src = 
	       case getc src of
		  NONE           => (false, src)
		| SOME (c, rest) => if c = #"." then (true, rest)
				    else (false, src)
	    fun esym src = 
	       case getc src of
		  NONE           => (false, src)
		| SOME (c, rest) => 
		     if c = #"e" orelse c = #"E"  then 
			(true, rest)
		     else (false, src)
	    fun scandigs first next final source =
	       let fun digs state src = 
		  case getc src of
		     NONE          => (SOME (final state), src)
		   | SOME (c, rest) => 
			if Char.isDigit c then 
			   digs (next (state, decval c)) rest
			else 
			   (SOME (final state), src)
	       in 
		  case getc source of
		     NONE          => (NONE, source)
		   | SOME (c, rest) => 
			if Char.isDigit c then digs (first (decval c)) rest
			else (NONE, source)
	       end

	    fun ident x = x
	    val getint  = 
	       scandigs real (fn (res, cval) => 10.0 * res + real cval) ident
	    val getfrac = 
	       scandigs (fn cval => (1, real cval))    
	       (fn ((decs, frac), cval) => (Int.+ (decs, 1), 10.0*frac+real cval))
	       (fn (decs, frac) => frac / pow10 decs)
	    val getexp =
	       scandigs ident (fn (res, cval) => Int.+ (Int.* (10, res), cval)) ident

	    fun sign src =
	       case getc src of
		  SOME (#"+", rest) => (true,  rest)
		| SOME (#"-", rest) => (false, rest)
		| SOME (#"~", rest) => (false, rest)
		| _                => (true,  src )

	    fun sym src =
	       case getc src of
		  SOME (#"i", restA) => 
		    (case Reader.reader2 getc restA of
		       SOME ((#"n", #"f"), restB) =>
			 SOME (posInf, 
			       case Reader.readerN (getc, 5) restB of
				 SOME ([#"i", #"n", #"i", #"t", #"y"], restC) => restC
			       | _ => restB)
		     | _ => NONE)
		| SOME (#"n", restA) =>
		    (case Reader.reader2 getc restA of
		       SOME ((#"a", #"n"), restB) =>
			 SOME (nan, restB)
		     | _ => NONE)
		| _ => NONE

	    val src = StringCvt.dropl Char.isSpace getc source
	    val (manpos, src1) = sign src
	    val (intg,   src2) = getint src1
	    val (decpt,  src3) = pointsym src2
	    val (frac,   src4) = getfrac src3 

	    fun mkres v rest = 
	       SOME (if manpos then v else ~v, rest)

	    fun expopt manval src = 
	       let val (esym,   src1) = esym src
		  val (exppos, src2) = sign src1
		  val (expv,   rest) = getexp src2 
	       in 
		  case (esym, expv) of
		     (_,     NONE)     => mkres manval src
		   | (true,  SOME exp) => 
			if exppos then mkres (manval * pow10 exp) rest
			else mkres (manval / pow10 exp) rest
		   | _                 => NONE
	       end
	 in 
	    case (intg,     decpt, frac) of
	       (NONE,      true,  SOME fval) => expopt fval src4
	     | (SOME ival, false, SOME _   ) => NONE
	     | (SOME ival, true,  NONE     ) => mkres ival src2
	     | (SOME ival, false, NONE     ) => expopt ival src2
	     | (SOME ival, _    , SOME fval) => expopt (ival+fval) src4
	     | _                             => (case sym src1 of
						   SOME (v, rest) => mkres v rest
						 | NONE => NONE)
	 end

      fun fromString s = StringCvt.scanString scan s

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
	       val mode = getRoundingMode ()
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
				  andalso Real.== (frac, 0.5)
				  then
				     if Real.== (0.5, realMod (whole / 2.0))
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

      val toDecimal = fn _ => raise (Fail "<Real.toDecimal not implemented>")
      val fromDecimal = fn _ => raise (Fail "<Real.fromDecimal not implemented>")
      val nextAfter = fn _ => raise (Fail "<Real.nextAfter not implemented>")
  end

structure Real = Real64   
structure LargeReal = Real64
structure RealGlobal: REAL_GLOBAL = Real
open RealGlobal
val real = Real.fromInt
