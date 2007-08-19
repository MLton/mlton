(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Time: TIME_EXTRA =
struct

structure Prim = PrimitiveFFI.Time

(* A time is represented as a number of nanoseconds. *)
val ticksPerSecond: LargeInt.int = 1000000000

datatype time = T of LargeInt.int

val fromTicks = T

exception Time

val zeroTime = T 0

fun fromReal r =
   T (LargeReal.toLargeInt IEEEReal.TO_NEAREST 
      (LargeReal.* (r, LargeReal.fromLargeInt ticksPerSecond)))
   handle Overflow => raise Time

fun toReal (T i) =
   LargeReal./ (LargeReal.fromLargeInt i, 
                LargeReal.fromLargeInt ticksPerSecond)

local
   fun make ticksPer =
      let
         val d = LargeInt.quot (ticksPerSecond, ticksPer)
      in
         (fn i => T (LargeInt.* (i, d)),
          fn T i => LargeInt.quot (i, d))
      end
in
   val (fromSeconds, toSeconds) = make 1
   val (fromMilliseconds, toMilliseconds) = make 1000
   val (fromMicroseconds, toMicroseconds) = make 1000000
   val (fromNanoseconds, toNanoseconds) = make 1000000000
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
local
   fun make f (T i, T i') = T (f (i, i'))
in
   val timeAdd = make LargeInt.+
   val timeSub = make LargeInt.-
end

(* There's a mess here to work around a bug in vmware virtual machines
 * that may return a decreasing(!) sequence of time values.  This will
 * cause some programs to raise Time exceptions where it should be
 * impossible.
 *)
local
   fun getNow (): time =
      (if ~1 = Prim.getTimeOfDay ()
          then raise Fail "Time.now"
       else ()
       ; timeAdd(fromSeconds (C_Time.toLargeInt (Prim.sec ())),
                 fromMicroseconds (C_SUSeconds.toLargeInt (Prim.usec ()))))
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
   fn n => (LargeReal.fmt (StringCvt.FIX (SOME n))) o toReal

val toString = fmt 3

(* Adapted from the ML Kit 4.1.4; basislib/Time.sml
 * by mfluet@acm.org on 2005-11-10 based on
 * by mfluet@acm.org on 2005-8-10 based on
 *  adaptations from the ML Kit 3 Version; basislib/Time.sml
 *  by sweeks@research.nj.nec.com on 1999-1-3.
 *)
fun scan getc src =
   let
      val charToDigit = StringCvt.charToDigit StringCvt.DEC
      fun pow10 0 = 1
        | pow10 n = 10 * pow10 (n-1)
      fun mkTime sign intv fracv decs =
         let
            val nsec = 
               LargeInt.div (LargeInt.+ (LargeInt.* (Int.toLarge (pow10 (10 - decs)), 
                                                     Int.toLarge fracv),
                                         5), 
                             10)
            val t =
               LargeInt.+ (LargeInt.* (Int.toLarge intv, ticksPerSecond),
                           nsec)
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
          | SOME (#".", rest) => frac sign 0 rest
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

val op + = timeAdd
val op - = timeSub

end
