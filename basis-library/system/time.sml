(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Time: TIME_EXTRA =
   struct
      structure Prim = Primitive.Time

      (* Inv: 0 <= usec < 1000000 *)
      datatype time = T of {sec: Int.int,
                            usec: Int.int}
      datatype time' = datatype time

      exception Time
      val thousand'': IntInf.int = 1000
      val thousand': LargeInt.int = 1000
      val thousand: int = 1000
      val million'': IntInf.int = 1000000
      val million': LargeInt.int = 1000000
      val million: int = 1000000
      
      val zeroTime = T {sec = 0,
			usec = 0}

      fun fromReal (r: LargeReal.real): time =
         let
	    val sec = LargeReal.floor r
	    val usec = LargeReal.floor (1E6 * (r - (LargeReal.fromInt sec)))
	 in T {sec = sec, usec = usec}
	 end handle Overflow => raise Time

      fun toReal (T {sec, usec}): LargeReal.real =
	 LargeReal.fromInt sec + (LargeReal.fromInt usec / 1E6)
	 
      fun toSeconds (T {sec, ...}) =
	 LargeInt.fromInt sec

      fun toMilliseconds (T {sec, usec}): LargeInt.int =
	 thousand' * LargeInt.fromInt sec
	 + LargeInt.fromInt (Int.quot (usec, thousand))
	 
      fun toMicroseconds (T {sec, usec}): LargeInt.int =
	 million' * LargeInt.fromInt sec + LargeInt.fromInt usec

      fun convert (s: LargeInt.int): int =
	 LargeInt.toInt s handle Overflow => raise Time
	    
      fun fromSeconds (s: LargeInt.int): time =
	 T {sec = convert s, usec = 0}

      fun fromMilliseconds (msec: LargeInt.int): time =
	let
	  val msec = IntInf.fromLarge msec
	  val (sec, msec) = IntInf.divMod (msec, thousand'')
	  val (sec, msec) = (IntInf.toLarge sec, IntInf.toLarge msec)
	in
	  T {sec = convert sec,
	     usec = (LargeInt.toInt msec) * thousand}
	end
 
      fun fromMicroseconds (usec: LargeInt.int): time =
	let
	  val usec = IntInf.fromLarge usec
	  val (sec, usec) = IntInf.divMod (usec, million'')
	  val (sec, usec) = (IntInf.toLarge sec, IntInf.toLarge usec)
	in
	  T {sec = convert sec,
	     usec = LargeInt.toInt usec}
	end
	 
      val add =
	 fn (T {sec = s, usec = u}, T {sec = s', usec = u'}) =>
	 let
	    val s'' = s + s' (* overflow possible *)
	    val u'' = u +? u'
	    val (s'', u'') =
	       if u'' >= million
		  then (s'' + 1, (* overflow possible *)
			u'' -? million)
	       else (s'', u'')
	 in T {sec = s'', usec = u''}
	 end
      (* Basis spec says Overflow, not Time, should be raised. *)
      (* handle Overflow => raise Time *) 

      val sub =
         fn (T {sec = s, usec = u}, T {sec = s', usec = u'}) =>
         let
	    val s'' = s - s' (* overflow possible *)
	    val u'' = u -? u'
	    val (s'', u'') =
	       if u'' < 0
		  then (s'' - 1, (* overflow possible *)
			u'' +? million)
	       else (s'', u'')
	 in T {sec = s'', usec = u''}
	 end
      (* Basis spec says Overflow, not Time, should be raised. *)
      (* handle Overflow => raise Time *) 

      fun compare (T {sec = s, usec = u}, T {sec = s', usec = u'}) =
	 if s > s'
	    then GREATER
	 else if s = s'
		 then Int.compare (u, u')
	      else (* s < s' *) LESS

      fun now (): time =
	 (Prim.gettimeofday ()
	  ; T {sec = Prim.sec (), usec = Prim.usec ()})

      fun fmt (digits: int) (t: time): string =
	 Real.fmt
	 (StringCvt.FIX (SOME (if digits < 0 then 0 else digits)))
	 (toReal t)

      val toString = fmt 3

      (* Adapted from MLKitV3 basislib/Time.sml*)
      fun scan getc src =
	 let
	    val charToDigit = StringCvt.charToDigit StringCvt.DEC
	    fun pow10 0 = 1
	      | pow10 n = 10 * pow10 (n-1)
	    fun mkTime sign intv fracv decs =
	       let
		  val sec = intv
		  val usec = (pow10 (7-decs) * fracv + 5) div 10
		  val t = T {sec = intv, usec = usec}
	       in 
		 if sign then t else sub (zeroTime, t)
	       end
	    fun frac' sign intv fracv decs src =
	       if decs >= 7 
		  then SOME (mkTime sign intv fracv decs, 
			     StringCvt.dropl Char.isDigit getc src)
	       else case getc src of
		       NONE           => SOME (mkTime sign intv fracv decs, src)
		     | SOME (c, rest) =>
                         (case charToDigit c of
			     NONE   => SOME (mkTime sign intv fracv decs, src)
			   | SOME d => frac' sign intv (10 * fracv + d) (decs + 1) rest)
	    fun frac sign intv src =
	       case getc src of
		 NONE           => NONE
	       | SOME (c, rest) =>
		   (case charToDigit c of
		      NONE   => NONE
		    | SOME d => frac' sign intv d 1 rest)
	    fun int' sign intv src =
	       case getc src of
		  NONE              => SOME (mkTime sign intv 0 7, src)
		| SOME (#".", rest) => frac sign intv rest
		| SOME (c, rest)    =>
		    (case charToDigit c of
		       NONE   => SOME (mkTime sign intv 0 7, src)
		     | SOME d => int' sign (10 * intv + d) rest)
	    fun int sign src =
	      case getc src of
		NONE           => NONE
	      | SOME (c, rest) => 
		  (case charToDigit c of
		     NONE   => NONE
		   | SOME d => int' sign d rest)
	 in 
	    case getc (StringCvt.skipWS getc src) of
	      NONE              => NONE
	    | SOME (#"+", rest) => int true rest
	    | SOME (#"~", rest) => int false rest
	    | SOME (#"-", rest) => int false rest
	    | SOME (#".", rest) => frac true 0 rest
	    | SOME (c, rest)    => 
                (case charToDigit c of
		   NONE => NONE
		 | SOME d => int' true d rest)
	 end
      val fromString = StringCvt.scanString scan

      val op + = add
      val op - = sub
      val {<, <=, >, >=} = Util.makeOrder compare
   end
