(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Time: TIME_EXTRA =
   struct
      structure Prim = Primitive.Time
	 
      (* Inv: sec >= 0 and 0 <= usec < 1000000 *)
      datatype time = T of {sec: Int.int, usec: Int.int}
      datatype time' = datatype time

      exception Time
      val thousand: int = 1000
      val million: int = 1000000
      
      val zeroTime = T {sec = 0, usec = 0}
	 
      fun fromReal (r: LargeReal.real): time  =
	 if r < 0.0
	    then raise Time
	 else let
		 val sec = LargeReal.floor r
		 val usec = LargeReal.floor (1E6 * (r - (LargeReal.fromInt sec)))
	      in T {sec = sec, usec = usec}
	      end handle Overflow => raise Time
		 
      fun toReal (T {sec, usec}): LargeReal.real =
	 LargeReal.fromInt sec + (LargeReal.fromInt usec / 1E6)
	 
      fun toSeconds (T {sec, ...}) =
	 LargeInt.fromInt sec

      fun toMilliseconds (T {sec, usec}): LargeInt.int =
	 1000 * LargeInt.fromInt sec
	 + LargeInt.fromInt (Int.quot (usec, thousand))
	 
      fun toMicroseconds (T {sec, usec}): LargeInt.int =
	 1000000 * LargeInt.fromInt sec + LargeInt.fromInt usec

      fun convert (s: LargeInt.int): int =
	 LargeInt.toInt s handle Overflow => raise Time
	    
      fun fromSeconds (s: LargeInt.int): time =
	 if Primitive.safe andalso s < 0
	    then raise Time
	 else T {sec = convert s, usec = 0}
      
      fun fromMilliseconds (ms: LargeInt.int): time =
	 if Primitive.safe andalso ms < 0
	    then raise Time
	 else
	    let
	       val (sec, ms) = IntInf.quotRem (ms, 1000)
	    in
	       T {sec = convert sec,
		  usec = LargeInt.toInt ms * 1000}
	    end
	    
      fun fromMicroseconds (usec: LargeInt.int): time =
	 if Primitive.safe andalso usec < 0
	    then raise Time
	 else
	    let
	       val (sec, usec) = IntInf.quotRem (usec, 1000000)
	    in
	       T {sec = convert sec,
		  usec = LargeInt.toInt usec}
	    end
	 
      val add =
	 fn (T {sec = s, usec = u}, T {sec = s', usec = u'}) =>
	 let
	    val u'' = u +? u'
	    val s'' = s + s'
	    val (s'', u'') =
	       if u'' >= million
		  then (s'' + 1, u'' -? million)
	       else (s'', u'')
	 in T {sec = s'', usec = u''}
	 end handle Overflow => raise Time
		 
      val sub =
	 fn (t1 as T {sec = s, usec = u}, t2 as T {sec = s', usec = u'}) =>
	 let
	    val s'' = s -? s'
	    val u'' = u -? u'
	    val (s'', u'') =
	       if u'' < 0
		  then (s'' -? 1, u'' +? million)
	       else (s'', u'')
	 in
	    if s'' < 0
	       then raise Time
	    else T {sec = s'', usec = u''}
	 end

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

      (* Copied from MLKitV3 basislib/Time.sml*)
      fun scan getc source =
	 let
	    fun skipWSget getc source = 
	       getc (StringCvt.dropl Char.isSpace getc source)
	     fun decval c = Char.ord c -? 48;
	     fun pow10 0 = 1
	       | pow10 n = 10 * pow10 (n-1)
	     fun mktime intgv decs fracv =
		let val usecs = (pow10 (7-decs) * fracv + 5) div 10
		in
		   T {sec = floor (intgv + 0.5) + usecs div 1000000, 
		     usec = usecs mod 1000000}
		end
	     fun skipdigs src =
		case getc src of 
		   NONE          => src
		 | SOME (c, rest) => if Char.isDigit c then skipdigs rest 
				    else src
	     fun frac intgv decs fracv src =
		if decs >= 7 then SOME (mktime intgv decs fracv, skipdigs src)
		else case getc src of
		   NONE          => SOME (mktime intgv decs fracv, src)
		 | SOME (c, rest) => 
		      if Char.isDigit c then 
			 frac intgv (decs+1) (10 * fracv + decval c) rest
		      else 
			 SOME (mktime intgv decs fracv, src)
	     fun intg intgv src = 
		case getc src of
		   NONE              => SOME (mktime intgv 6 0, src)
		 | SOME (#".", rest) => frac intgv 0 0 rest
		 | SOME (c, rest)    => 
		      if Char.isDigit c then 
			 intg (10.0 * intgv + real (decval c)) rest 
		      else SOME (mktime intgv 6 0, src)
	 in case skipWSget getc source of
	    NONE             => NONE
	  | SOME (#".", rest) => 
	       (case getc rest of
		   NONE          => NONE
		 | SOME (c, rest) => 
		      if Char.isDigit c then frac 0.0 1 (decval c) rest
		      else NONE)
	  | SOME (c, rest)    => 
	       if Char.isDigit c then intg (real (decval c)) rest else NONE
	 end

      val fromString = StringCvt.scanString scan

      val op + = add
      val op - = sub
      val {<, <=, >, >=} = Util.makeOrder compare
   end
