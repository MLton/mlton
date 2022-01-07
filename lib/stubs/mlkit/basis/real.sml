(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Real : REAL =
struct
   open Real
   val minPos = 4.9406564584124654E~324;
   val minNormalPos = 2.2250738585072014E~308;
   val maxFinite = 1.7976931348623157E308;
   fun *+ (a, b, c) = a * b + c
   fun *- (a, b, c) = a * b - c
   fun ?= (x, y) =
      if isNan x then true
      else if isNan y then true
      else == (x, y)
   fun checkFloat x =
      if isNan x then raise Div
      else if not (isFinite x) then raise Overflow
      else x
   fun class x =
      if isNan x then IEEEReal.NAN
      else if not (isFinite x) then IEEEReal.INF
      else if == (x, 0.0) then IEEEReal.ZERO
      else if (abs x) < minPos then IEEEReal.SUBNORMAL
      else IEEEReal.NORMAL
   fun compareReal (x, y) =
      (case compare (x, y) of
          LESS => IEEEReal.LESS
        | EQUAL => IEEEReal.EQUAL
        | GREATER => IEEEReal.GREATER)
      handle IEEEReal.Unordered => IEEEReal.UNORDERED
   fun copySign (x: real, y: real) : real = raise Fail "Real.copySign"
   fun fromDecimal d = raise Fail "Real.fromDecimal"
   fun fromLarge (_ : IEEEReal.rounding_mode) (r : LargeReal.real) : real = r
   fun fromLargeInt i = valOf (fromString (LargeInt.toString i))
   fun fromManExp {man: real, exp: int} : real = raise Fail "Real.fromManExp"
   fun isNormal x = (class x = IEEEReal.NORMAL)
   fun nextAfter (x: real, y: real) : real = raise Fail "Real.nextAfter"
   val precision = 53
   val radix = 2
   fun realCeil (r : real) : real = raise Fail "Real.realCeil"
   fun realFloor (_ : real) : real = raise Fail "Real.realFloor"
   fun realMod (_ : real) : real = raise Fail "Real.realMod"
   fun realRound (_ : real) : real = raise Fail "Real.realRound"
   fun realTrunc (_ : real) : real = raise Fail "Real.realTrunc"
   fun rem (_ : real, _ : real) : real = raise Fail "Real.rem"
   fun signBit r = sameSign (r, ~1.0)
   fun split (_ : real) : {whole : real, frac : real} = raise Fail "Real.split"
   fun toDecimal (_ : real) : IEEEReal.decimal_approx = raise Fail "Real.toDecimal"
   fun toInt rm r =
      case rm of
         IEEEReal.TO_NEGINF => floor r
       | IEEEReal.TO_POSINF => ceil r
       | IEEEReal.TO_ZERO => trunc r
       | IEEEReal.TO_NEAREST => round r
   fun toLarge (r: real) : LargeReal.real = r
   fun toLargeInt rm r = raise Fail "Real.toLargeInt"
   fun toManExp (_ : real) : {man: real, exp: int} = raise Fail "Real.toManExp"
   fun unordered (x, y) = isNan x orelse isNan y
end
