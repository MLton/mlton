(* scan is from ML Kit Version 3 basislib/real.sml *)

(* Some of this code was taken from SML/NJ real64.sml *)
(* real64.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Real: REAL =
   struct
      structure Real = Primitive.Real
      open Real IEEEReal
      infix 4 == != ?=
      type real = real

      local
	 (* reify is to eliminate Intel extra precision bits. *)
	 local
	    val r = ref 0.0
	 in fun reify (z: real): real = (r := z; !r)
	 end
      
	 fun min (p: real -> bool): real =
	    let
	       fun loop (x: real): real =
		  let val y = reify (x / 2.0)
		  in if p y
			then loop y
		     else x
		  end
	    in loop 1.0
	    end
      in
	 val minNormalPos = min isNormal
	 val minPos = min (fn x => x > 0.0)

	 val maxFinite =
	    let
	       fun up (x: real): real =
		  let val y = reify (x * 2.0)
		  in if isFinite y
			then up (y)
		     else x
		  end
	       fun down (x: real, y: real): real =
		  let val y = y / 2.0
		     val z = reify (x + y)
		  in if isFinite z
			then down (z, y)
		     else x
		  end
	       val z = up 1.0
	    in down (z, z)
	    end
      end
   
(*       local
 * 	 val r1 = ref 0.0
 * 	 val r2 = ref 0.0
 * 	 val r3 = ref 0.0 
 * 	 fun wrap f (x, y) =
 * 	    (r1 := x;
 * 	     r2 := y;
 * 	     let val z = f (!r1, !r2)
 * 	     in r3 := z ; !r3
 * 	     end)
 * 	 fun wrap f = f
 *       in val op + = wrap (op +)
 * 	 val op - = wrap (op -)
 * 	 val op * = wrap (op * )
 * 	 val op / = wrap (op /)
 *       end
 *)
      val radix: int = 2

      val precision: int = 52

      val posInf = 1.0 / 0.0
      val negInf = ~1.0 / 0.0

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
	    0 => NAN QUIET
	  | 1 => NAN SIGNALLING
	  | 2 => INF
	  | 3 => ZERO
	  | 4 => NORMAL
	  | 5 => SUBNORMAL
	  | _ => raise Fail "Primitive.Real.class returned bogus integer"

      local
	 val r: int ref = ref 0
      in
	 fun toManExp x =
	    let val man = frexp (x, r)
	    in {man = man, exp = !r}
	    end
      end

      fun fromManExp {man, exp} = ldexp (man, exp)

      local
	 val int = ref 0.0
      in
	 fun split x =
	    let val frac = modf (x, int)
	    in {frac = frac,
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
	 let val m' = getRoundingMode ()
	 in setRoundingMode m ;
	    th () before setRoundingMode m'
	 end

      val maxInt = fromInt Int.maxInt'
      val minInt = fromInt Int.minInt'

      fun toInt mode x =
	 let fun doit () = withRoundingMode (mode, fn () =>
					   Real.toInt (Real.round x))
	 in case class x of
	    NAN _ => raise Domain
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

(*       val toLargeInt = toInt
 *       val fromLargeInt = fromInt
 *)

      fun toLarge x = x
      fun fromLarge _ x = x
      
      val floor = toInt TO_NEGINF
      val ceil = toInt TO_POSINF
      val trunc = toInt TO_ZERO
      val round = toInt TO_NEAREST

      local
	 fun round mode x =
	    case class x of
	       NAN _ => x
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
		  NAN _ => "nan" (* this is wrong *)
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
			   (Primitive.String.fromCharVector
			    (Array.extract (buffer, 0, SOME len)))
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
	     | _                             => NONE 
	 end

      fun fromString s = StringCvt.scanString scan s
   end

structure RealGlobal: REAL_GLOBAL = Real
open RealGlobal
val real = Real.fromInt
   
structure LargeReal: REAL = Real
