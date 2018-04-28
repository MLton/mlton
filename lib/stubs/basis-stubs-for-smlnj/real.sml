(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* SML/NJ uses an old version of datatype IEEEReal.float_class. *)
signature REAL =
   sig
      type real

      structure Math: MATH where type real = real

      val != : real * real -> bool
      val * : real * real -> real
      val *+ : real * real * real -> real
      val *- : real * real * real -> real
      val + : real * real -> real
      val - : real * real -> real
      val / : real * real -> real
      val <  : real * real -> bool
      val <= : real * real -> bool
      val == : real * real -> bool
      val >  : real * real -> bool
      val >= : real * real -> bool
      val ?= : real * real -> bool
      val abs: real -> real
      val checkFloat: real -> real
      val class: real -> IEEEReal.float_class
      val compare: real * real -> order
      val compareReal: real * real -> IEEEReal.real_order
      val copySign: real * real -> real
      val fmt: StringCvt.realfmt -> real -> string
      val fromDecimal: IEEEReal.decimal_approx -> real option
      val fromInt: int -> real
      val fromLarge: IEEEReal.rounding_mode -> LargeReal.real -> real
      val fromLargeInt: LargeInt.int -> real
      val fromManExp: {man: real, exp: int} -> real
      val fromString: string -> real option
      val isFinite: real -> bool
      val isNan: real -> bool
      val isNormal: real -> bool
      val max: real * real -> real
      val maxFinite: real
      val min: real * real -> real
      val minNormalPos: real
      val minPos: real
      val negInf: real
      val nextAfter: real * real -> real
      val posInf: real
      val precision: int
      val radix: int
      val realCeil: real -> real
      val realFloor: real -> real
      val realMod: real -> real
      val realRound: real -> real
      val realTrunc: real -> real
      val rem: real * real -> real
      val round: real -> Int.int
      val sameSign: real * real -> bool
      val scan: (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
      val sign: real -> int
      val signBit: real -> bool
      val split: real -> {whole: real, frac: real}
      val toDecimal: real -> IEEEReal.decimal_approx
      val toInt: IEEEReal.rounding_mode -> real -> int
      val toLarge: real -> LargeReal.real
      val toLargeInt: IEEEReal.rounding_mode -> real -> LargeInt.int
      val toManExp: real -> {man: real, exp: int}
      val toString: real -> string
      val unordered: real * real -> bool
      val ~ : real -> real
      val ceil: real -> Int.int
      val floor: real -> Int.int
      val trunc: real -> Int.int
   end

functor FixReal(PReal: sig include PERVASIVE_REAL val zero : real end) : REAL =
   struct
      open PReal

      local
         datatype z = datatype IEEEReal.float_class
         structure P = Pervasive.IEEEReal
         fun toGoodFC c =
            case c of
               P.NAN _ => NAN
             | P.INF => INF
             | P.ZERO => ZERO
             | P.NORMAL => NORMAL
             | P.SUBNORMAL => SUBNORMAL
         fun toBadFC c =
            case c of
               NAN => P.NAN P.QUIET
             | INF => P.INF
             | ZERO => P.ZERO
             | NORMAL => P.NORMAL
             | SUBNORMAL => P.SUBNORMAL
         fun toGoodDA {digits, exp, kind, sign} =
            {digits = digits, exp = exp, kind = toGoodFC kind, sign = sign}
         fun toBadDA {digits, exp, kind, sign} =
            {digits = digits, exp = exp, kind = toBadFC kind, sign = sign}
      in
         val class = toGoodFC o class
         val fromDecimal = SOME o fromDecimal o toBadDA
         val toDecimal = toGoodDA o toDecimal
      end

      (* SML/NJ doesn't support EXACT
       * and doesn't include a leading "~" for ~0.0.
       *)
      fun fmt f =
         let
            val fmt =
               PReal.fmt
               (let
                   datatype z = datatype StringCvt.realfmt
                in
                   case f of
                      EXACT => StringCvt.GEN NONE
                    | FIX io => StringCvt.FIX io
                    | GEN io => StringCvt.GEN io
                    | SCI io => StringCvt.SCI io
                end)
         in
            fn r =>
            if == (zero, r) andalso signBit r
               then "~" ^ (fmt r)
            else fmt r
         end

      (* SML/NJ doesn't handle "[+~-]?(inf|infinity|nan)"
       * and raises Overflow on large exponents.
       *)
      fun fromString s =
         case s of
            "inf" => SOME posInf
          | "infinity" => SOME posInf
          | "+inf" => SOME posInf
          | "+infinity" => SOME posInf
          | "~inf" => SOME negInf
          | "~infinity" => SOME negInf
          | "-inf" => SOME negInf
          | "-infinity" => SOME negInf
          | "nan" => SOME (negInf + posInf)
          | "+nan" => SOME (negInf + posInf)
          | "~nan" => SOME (negInf + posInf)
          | "-nan" => SOME (negInf + posInf)
          | _ =>
               (case SOME (PReal.fromString s) handle Overflow => NONE of
                   NONE =>
                      let
                         val manexp =
                            String.tokens
                            (fn c => c = #"e" orelse c = #"E")
                            s
                         fun isNeg s =
                            String.sub (s, 0) = #"~"
                            orelse String.sub (s, 0) = #"+"
                         fun isNonzero s =
                            CharVector.exists
                            (fn c => Char.<= (#"1", c) andalso Char.<= (c, #"9"))
                            s
                      in
                         case manexp of
                            [man,exp] =>
                               if isNeg exp
                                  then SOME zero
                               else if isNonzero man
                                  then SOME posInf
                               else SOME zero
                           | _ => NONE
                      end
                 | SOME ro => ro)
   end

structure LargeReal = FixReal(struct open Pervasive.LargeReal val zero : real = 0.0 end)
structure Real = FixReal(struct open Pervasive.Real val zero : real = 0.0 end)
structure Real64 = FixReal(struct open Pervasive.Real64 val zero : real = 0.0 end)
structure Real32 = Real64

(* Dummy implementation that will not be used at run-time. *)
structure PackReal32Big : PACK_REAL where type real = Real32.real = struct
   type real = Real32.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal32Big.toBytes"
   fun fromBytes _ = raise Fail "PackReal32Big.fromBytes"
   fun subVec _ = raise Fail "PackReal32Big.subVec"
   fun subArr _ = raise Fail "PackReal32Big.subArr"
   fun update _ = raise Fail "PackReal32Big.update"
end
(* Dummy implementation that will not be used at run-time. *)
structure PackReal32Little : PACK_REAL where type real = Real32.real = struct
   type real = Real32.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal32Little.toBytes"
   fun fromBytes _ = raise Fail "PackReal32Little.fromBytes"
   fun subVec _ = raise Fail "PackReal32Little.subVec"
   fun subArr _ = raise Fail "PackReal32Little.subArr"
   fun update _ = raise Fail "PackReal32Little.update"
end

(* Dummy implementation that will not be used at run-time. *)
structure PackReal64Big : PACK_REAL where type real = Real64.real  = struct
   type real = Real64.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal64Big.toBytes"
   fun fromBytes _ = raise Fail "PackReal64Big.fromBytes"
   fun subVec _ = raise Fail "PackReal64Big.subVec"
   fun subArr _ = raise Fail "PackReal64Big.subArr"
   fun update _ = raise Fail "PackReal64Big.update"
end
(* Dummy implementation that will not be used at run-time. *)
structure PackReal64Little : PACK_REAL where type real = Real64.real  = struct
   type real = Real64.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal64Little.toBytes"
   fun fromBytes _ = raise Fail "PackReal64Little.fromBytes"
   fun subVec _ = raise Fail "PackReal64Little.subVec"
   fun subArr _ = raise Fail "PackReal64Little.subArr"
   fun update _ = raise Fail "PackReal64Little.update"
end
