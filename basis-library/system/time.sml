(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Time: TIME =
struct

structure Prim = Primitive.Time

(* A time is represented as a number of microseconds. *)
val precision: int = 6
val ticksPerSec: LargeInt.int = 1000000
   
datatype time = T of LargeInt.int 

exception Time

val zeroTime = T 0

fun fromReal r =
   T (Real.toLargeInt IEEEReal.TO_NEAREST (r * Real.fromLargeInt ticksPerSec))

fun toReal (T i) =
   Real.fromLargeInt i / Real.fromLargeInt ticksPerSec

local
   fun make ticksPer =
      let
	 val d = LargeInt.quot (ticksPerSec, ticksPer)
      in
	 (fn i => T (LargeInt.* (i, d)),
	  fn T i => LargeInt.quot (i, d))
      end
in
   val (fromSeconds, toSeconds) = make 1
   val (fromMilliseconds, toMilliseconds) = make 1000
   val (fromMicroseconds, toMicroseconds) = make 1000000
end

local
   fun make f (T i, T i') = f (i, i')
in
   val compare = make LargeInt.compare
   val op < = make LargeInt.<
   val op <= = make LargeInt.<=
   val op > = make LargeInt.>
   val op >= = make LargeInt.>=
end

(* There's a mess here to work around a bug in vmware virtual machines
 * that may return a decreasing(!) sequence of time values.  This will
 * cause some programs to raise Time exceptions where it should be
 * impossible.
 *)
local
   fun getNow (): time =
      (Prim.gettimeofday ()
       ; T (LargeInt.+ (LargeInt.* (LargeInt.fromInt (Prim.sec ()), ticksPerSec),
			LargeInt.fromInt (Prim.usec ()))))
   val prev = ref (getNow ())
in
   fun now (): time =
      let
	 val old = !prev
	 val t = getNow ()
      in
	 case compare (old, t) of
	    GREATER => old
	  | _ => (prev := t; t)
      end
end

val fmt: int -> time -> string =
   fn n => (Real.fmt (StringCvt.FIX (SOME n))) o toReal

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
	    val t =
	       LargeInt.+ (LargeInt.* (Int.toLarge intv, ticksPerSec),
			   Int.toLarge usec)
	    val t = if sign then t else LargeInt.~ t
	 in
	    T t
	 end
      fun frac' sign intv fracv decs src =
	 if Int.>= (decs, 7)
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
handle Overflow => raise Time

val fromString = StringCvt.scanString scan

local
   fun make f (T i, T i') = T (f (i, i'))
in
   val op + = make LargeInt.+
   val op - = make LargeInt.-
end

end
