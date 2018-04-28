(* Copyright (C) 2011-2014,2017 Matthew Fluet.
 * Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Real (structure W: WORD_EXTRA
              structure R:
                 sig
                    include PRE_REAL
                    val castToWord: real -> W.word
                    val castFromWord: W.word -> real
                 end): REAL_EXTRA =
   struct
      structure MLton = Primitive.MLton
      structure Prim = R
      local
         open IEEEReal
      in
         datatype float_class = datatype float_class
         datatype rounding_mode = datatype rounding_mode
      end
      infix 4 == != ?=
      type real = R.real

      local
         open Prim
      in
         val realSize = Int32.toInt realSize
         val exponentBias = Int32.toInt exponentBias
         val precision = Int32.toInt precision
         val radix = Int32.toInt radix
      end

      val signBits = Word.one
      val exponentSignificandBits = Word.- (Word.fromInt realSize, signBits)
      val significandBits = Word.- (Word.fromInt precision, Word.one)
      val exponentBits = Word.- (exponentSignificandBits, significandBits)

      local
         val mkMask : Word.word -> W.word =
            fn b => W.notb (W.<< (W.notb W.zero, b))
      in
         val signMask =
            W.<< (mkMask signBits, exponentSignificandBits)
         val exponentMask =
            W.<< (mkMask exponentBits, significandBits)
         val significandMask =
            mkMask significandBits
      end

      val class : real -> float_class =
         fn r =>
         let
            val w = R.castToWord r
         in
            if W.andb (w, exponentMask) = exponentMask
               then if W.andb (w, significandMask) = W.zero
                       then IEEEReal.INF
                    else IEEEReal.NAN
            else if W.andb (w, exponentMask) = W.zero
               then if W.andb (w, significandMask) = W.zero
                       then IEEEReal.ZERO
                    else IEEEReal.SUBNORMAL
            else IEEEReal.NORMAL
         end

      val toBits : real -> {sign: bool, exponent: W.word, significand: W.word} =
         fn r =>
         let
            val w = R.castToWord r
            val significand =
               W.andb (w, significandMask)
            val exponent =
               W.>> (W.andb (w, exponentMask), significandBits)
            val sign =
               W.andb (w, signMask) = signMask
         in
            {sign = sign,
             exponent = exponent,
             significand = significand}
         end

      val fromBits : {sign: bool, exponent: W.word, significand: W.word} -> real =
         fn {sign, exponent, significand} =>
         let
            val w =
               W.orb (if sign then W.<< (W.one, exponentSignificandBits) else W.zero,
                      W.orb (W.andb (W.<< (exponent, significandBits), exponentMask),
                             W.andb (significand, significandMask)))
            val r = R.castFromWord w
         in
            r
         end

      local
         open Prim
      in
         val op *+ = op *+
         val op *- = op *-
         val op * = op *
         val op + = op +
         val op - = op -
         val op / = op /
         val op < = op <
         val op <= = op <=
         val op > = op >
         val op >= = op >=
         val ~ = ~
         val abs = abs
      end

      local
         fun 'a make {fromRealUnsafe: 'a -> real,
                      toRealUnsafe: real -> 'a,
                      other : {precision: Primitive.Int32.int}} =
            if R.precision = #precision other
               then (fn (_: rounding_mode) => fromRealUnsafe,
                     toRealUnsafe)
               else (fn (m: rounding_mode) => fn r =>
                     IEEEReal.withRoundingMode (m, fn () => fromRealUnsafe r),
                     toRealUnsafe)
      in
         val (fromReal32,toReal32) =
            make {fromRealUnsafe = R.fromReal32Unsafe,
                  toRealUnsafe = R.toReal32Unsafe,
                  other = {precision = Primitive.Real32.precision}}
         val (fromReal64,toReal64) =
            make {fromRealUnsafe = R.fromReal64Unsafe,
                  toRealUnsafe = R.toReal64Unsafe,
                  other = {precision = Primitive.Real64.precision}}
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = real -> 'a
             val fReal32 = toReal32
             val fReal64 = toReal64)
      in
         val toLarge = S.f
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = rounding_mode -> 'a -> real
             val fReal32 = fromReal32
             val fReal64 = fromReal64)
      in
         val fromLarge = S.f
      end

      val negInf = R.castFromWord (W.orb (signMask, exponentMask))
      val negOne = R.castFromWord (W.orb (signMask, W.<< (W.fromInt exponentBias, significandBits)))
      val negZero = R.castFromWord signMask
      val zero = R.castFromWord W.zero
      val minPos = R.castFromWord W.one
      val minNormalPos = R.castFromWord (W.<< (W.one, significandBits))
      val half = R.castFromWord (W.<< (W.- (W.fromInt exponentBias, W.one), significandBits))
      val one = R.castFromWord (W.<< (W.fromInt exponentBias, significandBits))
      val two = R.castFromWord (W.<< (W.+ (W.fromInt exponentBias, W.one), significandBits))
      val maxFinite = R.castFromWord (W.- (exponentMask, W.one))
      val posInf = R.castFromWord exponentMask

      val nan = posInf + negInf
      val posNan = R.castFromWord (W.andb (R.castToWord nan, W.notb signMask))
      val negNan = R.castFromWord (W.orb (R.castToWord nan, signMask))


      fun isFinite r =
         abs r <= maxFinite

      val op == = Prim.==

      val op != = not o op ==

      fun isNan r = r != r

      fun isNormal r = class r = NORMAL

      val op ?= =
         if MLton.Codegen.isAMD64 orelse MLton.Codegen.isLLVM orelse MLton.Codegen.isX86
            then R.?=
         else
            fn (x, y) =>
            case (class x, class y) of
               (NAN, _) => true
             | (_, NAN) => true
             | (ZERO, ZERO) => true
             | _ => R.== (x, y)

      fun min (x, y) =
         if x <= y then x
         else if x > y then y
         else if isNan y then x
         else y

      fun max (x, y) =
         if x >= y then x
         else if x < y then y
         else if isNan y then x
         else y

      fun sign (x: real): int =
         if x > zero then 1
         else if x < zero then ~1
         else if x == zero then 0
         else raise Domain

      val signBit = #sign o toBits

      fun sameSign (x, y) = signBit x = signBit y

      fun copySign (x, y) =
         if sameSign (x, y)
            then x
         else ~ x

      local
         structure I = IEEEReal
      in
         fun compareReal (x, y) =
            if x < y then I.LESS
            else if x > y then I.GREATER
            else if x == y then I.EQUAL
            else I.UNORDERED
      end

      local
         structure I = IEEEReal
         structure G = General
      in
         fun compare (x, y) =
            case compareReal (x, y) of
               I.EQUAL => G.EQUAL
             | I.GREATER => G.GREATER
             | I.LESS => G.LESS
             | I.UNORDERED => raise IEEEReal.Unordered
      end

      fun unordered (x, y) = isNan x orelse isNan y

      (* nextAfter for subnormal and normal values works by converting
       * the real to a word of equivalent size and doing an increment
       * or decrement on the word.  Because of the way IEEE floating
       * point numbers are represented, word {de,in}crement
       * automatically does the right thing at the boundary between
       * normals and denormals.  Also, convienently,
       * maxFinite+1 = posInf and minFinite-1 = negInf.
       *)
      val nextAfter: real * real -> real =
         fn (r, t) =>
         case (class r, class t) of
            (NAN, _) => nan
          | (_, NAN) => nan
          | (INF, _) => r
          | (ZERO, ZERO) => t (* want "t", not "r", to get the sign right *)
          | (ZERO, _) => if t > zero then minPos else ~minPos
          | _ =>
               if r == t then
                  r
               else if (r > t) = (r > zero) then
                  R.castFromWord (W.- (R.castToWord r, W.one))
               else
                  R.castFromWord (W.+ (R.castToWord r, W.one))

      local
         val one = One.make (fn () => ref (0 : C_Int.t))
      in
         fun toManExp x =
            case class x of
               INF => {exp = 0, man = x}
             | NAN => {exp = 0, man = nan}
             | ZERO => {exp = 0, man = x}
             | _ => One.use (one, fn r =>
                             let
                                val man = R.frexp (x, r)
                             in
                                {exp = C_Int.toInt (!r), man = man}
                             end)
      end

      fun fromManExp {exp, man} =
         (R.ldexp (man, C_Int.fromInt exp))
         handle Overflow =>
            man * (if Int.< (exp, 0) then zero else posInf)

      val fromManExp =
         if MLton.Codegen.isX86
            then fromManExp
         else
            fn {exp, man} =>
            case class man of
               INF => man
             | NAN => man
             | ZERO => man
             | _ => fromManExp {exp = exp, man = man}

      local
         val one = One.make (fn () => ref zero)
      in
         fun split x =
            case class x of
               INF => {frac = if x > zero then zero else ~zero,
                       whole = x}
             | NAN => {frac = nan, whole = nan}
             | _ =>
                  let
                     val (frac, whole) =
                        One.use (one, fn int =>
                                 (R.modf (x, int), !int))
                     (* Some platforms' C libraries don't get sign of
                      * zero right.
                      *)
                     fun fix y =
                        if class y = ZERO andalso not (sameSign (x, y))
                           then ~ y
                           else y
                  in
                     {frac = fix frac,
                      whole = fix whole}
                  end
      end

      val realMod = #frac o split

      fun checkFloat x =
         if isFinite x then x
         else if isNan x then raise Div
         else raise Overflow

      val realCeil = R.realCeil
      val realFloor = R.realFloor
      val realTrunc = R.realTrunc
      
      (* Unfortunately, libc round ties to zero instead of even values. *)
      (* Fortunately, if any rounding mode is supported, it's TO_NEAREST. *)
      val realRound = fn r => IEEEReal.withRoundingMode (TO_NEAREST, fn () => R.round r)
      
      fun rem (x, y) =
         (case class x of
             INF => nan
           | NAN => nan
           | ZERO => zero
           | _ => (case class y of
                      INF => x
                    | NAN => nan
                    | ZERO => nan
                    | _ => x - realTrunc (x/y) * y))

      (* fromDecimal, scan, fromString: decimal -> binary conversions *)
      fun strtor (str: NullString.t,
                  rounding_mode: IEEEReal.rounding_mode) =
         let
            val rounding : C_Int.int =
               case rounding_mode of
                  TO_NEAREST => 1
                | TO_NEGINF => 3
                | TO_POSINF => 2
                | TO_ZERO => 0
         in
            Prim.strtor (str, rounding)
         end
      exception Bad
      fun fromDecimalWithRoundingMode
          ({class, digits, exp, sign}: IEEEReal.decimal_approx,
           rounding_mode: IEEEReal.rounding_mode) =
         let
            fun doit () =
               let
                  val exp =
                     if Int.< (exp, 0)
                        then concat ["-", Int.toString (Int.~ exp)]
                        else Int.toString exp
(*
                  val str = concat [if sign then "-" else "",
                                    "0.", digits,
                                    "E", exp, "\000"]
*)
                  val n = Int.+ (if sign then 1 else 0,
                          Int.+ (4 (* "0." + "E" + "\000" *),
                          Int.+ (List.length digits,
                                 String.size exp)))
                  val a = Array.alloc n
                  fun upd (i, c) = (Array.update (a, i, c); Int.+ (i, 1))
                  val i = 0
                  val i = if sign then upd (i, #"-") else i
                  val i = upd (i, #"0")
                  val i = upd (i, #".")
                  val i =
                     List.foldl
                     (fn (d, i) =>
                      if Int.< (d, 0) orelse Int.> (d, 9)
                         then raise Bad
                         else upd (i, Char.chr (Int.+ (d, Char.ord #"0"))))
                     i digits
                  val i = upd (i, #"E")
                  val i = CharVector.foldl (fn (c, i) => upd (i, c)) i exp
                  val _ = upd (i, #"\000")
                  val str = Vector.unsafeFromArray a
                  val x = strtor (NullString.fromString str, rounding_mode)
               in
                  x
               end
         in
            SOME (case class of
                     INF => if sign then negInf else posInf
                   | NAN => if sign then negNan else posNan
                   | NORMAL => doit ()
                   | SUBNORMAL => doit ()
                   | ZERO => if sign then negZero else zero)
            handle Bad => NONE
         end

      fun fromDecimal da = fromDecimalWithRoundingMode (da, TO_NEAREST)

      fun scan reader state =
         case IEEEReal.scan reader state of
            NONE => NONE
          | SOME (da, state) =>
               SOME (valOf (fromDecimalWithRoundingMode
                            (da, IEEEReal.getRoundingMode ())),
                     state)

      val fromString = StringCvt.scanString scan

      (* toDecimal, fmt, toString: binary -> decimal conversions. *)
      datatype mode = Fix | Gen | Sci
      local
         val one = One.make (fn () => ref (0: C_Int.int))
      in
         fun gdtoa (x: real, mode: mode, ndig: int,
                    rounding_mode: IEEEReal.rounding_mode) =
            let
               val mode : C_Int.int =
                  case mode of
                     Fix => 3
                   | Gen => 0
                   | Sci => 2
               val ndig : C_Int.int = C_Int.fromInt ndig
               val rounding : C_Int.int =
                  case rounding_mode of
                     TO_NEAREST => 1
                   | TO_NEGINF => 3
                   | TO_POSINF => 2
                   | TO_ZERO => 0
            in
               One.use (one, fn decpt =>
                        (Prim.gdtoa (x, mode, ndig, rounding, decpt),
                         C_Int.toInt (!decpt)))
            end
      end

      fun toDecimal (x: real): IEEEReal.decimal_approx =
         case class x of
            INF => {class = INF,
                    digits = [],
                    exp = 0,
                    sign = x < zero}
          | NAN => {class = NAN,
                    digits = [],
                    exp = 0,
                    sign = signBit x}
          | ZERO => {class = ZERO,
                     digits = [],
                     exp = 0,
                     sign = signBit x}
          | c =>
               let
                  val (cs, exp) = gdtoa (x, Gen, 0, TO_NEAREST)
                  fun loop (i, ac) =
                     if Int.< (i, 0)
                        then ac
                     else loop (Int.- (i, 1),
                                (Int.- (Char.ord (CUtil.C_String.sub (cs, i)),
                                        Char.ord #"0"))
                                :: ac)
                  val digits = loop (Int.- (CUtil.C_String.length cs, 1), [])
               in
                  {class = c,
                   digits = digits,
                   exp = exp,
                   sign = x < zero}
               end

      datatype realfmt = datatype StringCvt.realfmt

      local
         fun fix (sign: string, cs: CUtil.C_String.t, decpt: int, ndig: int): string =
            let
               val length = CUtil.C_String.length cs
            in
               if Int.< (decpt, 0)
                  then
                     concat [sign,
                             "0.",
                             String.new (Int.~ decpt, #"0"),
                             CUtil.C_String.toString cs,
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
                                               then CUtil.C_String.sub (cs, i)
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
                                     then CUtil.C_String.sub (cs, j)
                                  else #"0"
                               end)
                        in
                           concat [sign, whole, ".", frac]
                        end
                  end
            end
         fun sci (x: real, ndig: int): string =
            let
               val sign = if x < zero then "~" else ""
               val (cs, decpt) =
                  gdtoa (x, Sci, Int.+ (1, ndig), IEEEReal.getRoundingMode ())
               val length = CUtil.C_String.length cs
               val whole = String.tabulate (1, fn _ => CUtil.C_String.sub (cs, 0))
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
                                      then CUtil.C_String.sub (cs, j)
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
         fun gen (x: real, n: int): string =
            let
               val (prefix, x) =
                  if x < zero
                     then ("~", ~ x)
                  else ("", x)
               val ss = Substring.full (sci (x, Int.- (n, 1)))
               fun isE c = c = #"E"
               fun isZero c = c = #"0"
               val expS =
                  Substring.string (Substring.taker (not o isE) ss)
               val exp = valOf (Int.fromString expS)
               val man =
                  String.translate
                  (fn #"." => "" | c => str c)
                  (Substring.string (Substring.dropr isZero
                                     (Substring.takel (not o isE) ss)))
               val manSize = String.size man
               fun zeros i = CharVector.tabulate (i, fn _ => #"0")
               fun dotAt i =
                  concat [String.substring (man, 0, i),
                          ".", String.extract (man, i, NONE)]
               fun sci () = concat [prefix,
                                    if manSize = 1 then man else dotAt 1,
                                    "E", expS]
               val op - = Int.-
               val op + = Int.+
               val ~ = Int.~
               val op >= = Int.>=
            in
               if exp >= (if manSize = 1 then 3 else manSize + 3)
                  then sci ()
               else if exp >= manSize - 1
                  then concat [prefix, man, zeros (exp - (manSize - 1))]
               else if exp >= 0
                  then concat [prefix, dotAt (exp + 1)]
               else if exp >= (if manSize = 1 then ~2 else ~3)
                  then concat [prefix, "0.", zeros (~exp - 1), man]
               else sci ()
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
                                    if Primitive.Controls.safe andalso Int.< (n, 0)
                                       then raise Size
                                    else n
                        in
                           fn x =>
                           let
                              val sign = if x < zero then "~" else ""
                              val (cs, decpt) =
                                 gdtoa (x, Fix, n, IEEEReal.getRoundingMode ())
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
                                    if Primitive.Controls.safe andalso Int.< (n, 1)
                                       then raise Size
                                    else n
                        in
                           fn x => gen (x, n)
                        end
                   | SCI opt =>
                        let
                           val n =
                              case opt of
                                 NONE => 6
                               | SOME n =>
                                    if Primitive.Controls.safe andalso Int.< (n, 0)
                                       then raise Size
                                    else n
                        in
                           fn x => sci (x, n)
                        end
            in
               fn x =>
               case class x of
                  NAN => (* if signBit x then "~nan" else *) "nan"
                | INF => if x > zero then "inf" else "~inf"
                | _ => doit x
            end
      end

      val toString = fmt (StringCvt.GEN NONE)
      
      (* Not all devices support all rounding modes. 
       * However, every device has ceil/floor/round/trunc.
       *)
      fun safeConvert (m, cvt, x) =
         case m of
            TO_POSINF => cvt (realCeil x)
          | TO_NEGINF => cvt (realFloor x)
          | TO_NEAREST => cvt (realRound x)
          | TO_ZERO => cvt (realTrunc x)

      local
         fun 'a make {fromIntUnsafe: 'a -> real,
                      toIntUnsafe: real -> 'a,
                      other : {maxInt': Word.word -> 'a,
                               minInt': 'a,
                               precision': int}} =
            (fromIntUnsafe,
             if Int.< (precision, #precision' other) then
                let
                   val trim = Int.- (Int.- (#precision' other, precision), 1)
                   val maxInt' = (#maxInt' other) (Word.fromInt trim)
                   val minInt' = #minInt' other
                   val maxInt = fromIntUnsafe maxInt'
                   val minInt = fromIntUnsafe minInt'
                in
                   fn (m: rounding_mode) => fn x =>
                   if minInt <= x then
                      if x <= maxInt then
                         safeConvert (m, toIntUnsafe, x)
                      else
                         raise Overflow
                   else
                      if x < minInt then
                         raise Overflow
                      else
                         raise Domain (* NaN *)
                end
             else
                let
                   val maxInt' = (#maxInt' other) 0w0
                   val minInt' = #minInt' other
                   val maxInt = fromIntUnsafe maxInt'
                   val minInt = fromIntUnsafe minInt'
                in
                   fn (m: rounding_mode) => fn x =>
                   if minInt <= x then
                      if x <= maxInt then
                         safeConvert (m, toIntUnsafe, x)
                      else
                         if x < maxInt + one then
                            (case m of
                                TO_NEGINF => maxInt'
                              | TO_POSINF => raise Overflow
                              | TO_ZERO => maxInt'
                              | TO_NEAREST =>
                                (* Depends on maxInt being odd. *)
                                if x - maxInt >= half then
                                   raise Overflow
                                else
                                   maxInt')
                         else
                            raise Overflow
                   else
                      if x < minInt then
                         if minInt - one < x then
                            (case m of
                                TO_NEGINF => raise Overflow
                              | TO_POSINF => minInt'
                              | TO_ZERO => minInt'
                              | TO_NEAREST =>
                                (* Depends on minInt being even. *)
                                if x - minInt < ~half then
                                   raise Overflow
                                else
                                   minInt')
                         else
                            raise Overflow
                      else
                         raise Domain (* NaN *)
                end)
      in
         val (fromInt8,toInt8) =
            make {fromIntUnsafe = R.fromInt8Unsafe,
                  toIntUnsafe = R.toInt8Unsafe,
                  other = {maxInt' = fn w => Int8.<< (Int8.>> (Int8.maxInt', w), w),
                           minInt' = Int8.minInt',
                           precision' = Int8.precision'}}
         val (fromInt16,toInt16) =
            make {fromIntUnsafe = R.fromInt16Unsafe,
                  toIntUnsafe = R.toInt16Unsafe,
                  other = {maxInt' = fn w => Int16.<< (Int16.>> (Int16.maxInt', w), w),
                           minInt' = Int16.minInt',
                           precision' = Int16.precision'}}
         val (fromInt32,toInt32) =
            make {fromIntUnsafe = R.fromInt32Unsafe,
                  toIntUnsafe = R.toInt32Unsafe,
                  other = {maxInt' = fn w => Int32.<< (Int32.>> (Int32.maxInt', w), w),
                           minInt' = Int32.minInt',
                           precision' = Int32.precision'}}
         val (fromInt64,toInt64) =
            make {fromIntUnsafe = R.fromInt64Unsafe,
                  toIntUnsafe = R.toInt64Unsafe,
                  other = {maxInt' = fn w => Int64.<< (Int64.>> (Int64.maxInt', w), w),
                           minInt' = Int64.minInt',
                           precision' = Int64.precision'}}
      end

      val fromIntInf: IntInf.int -> real =
         fn i =>
         let
            val str =
               if IntInf.< (i, 0)
                  then "-" ^ (IntInf.toString (IntInf.~ i))
               else IntInf.toString i
            val x = strtor (NullString.nullTerm str,
                            IEEEReal.getRoundingMode ())
         in
            x
         end

      val toIntInf: rounding_mode -> real -> LargeInt.int =
         fn mode => fn x =>
         case class x of
            INF => raise Overflow
          | NAN => raise Domain
          | ZERO => (0 : LargeInt.int)
          | _ =>
               let
                  (* This round may turn x into an INF, so we need to check the
                   * class again.
                   *)
                  val x = 
                     case mode of
                        TO_POSINF => realCeil x
                      | TO_NEGINF => realFloor x
                      | TO_NEAREST => realRound x
                      | TO_ZERO => realTrunc x
               in
                  case class x of
                     INF => raise Overflow
                   | _ => valOf (IntInf.fromString (fmt (StringCvt.FIX (SOME 0)) x))
               end

      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> real
             val fInt8 = fromInt8
             val fInt16 = fromInt16
             val fInt32 = fromInt32
             val fInt64 = fromInt64
             val fIntInf = fromIntInf)
      in
         val fromInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> real
             val fInt8 = fromInt8
             val fInt16 = fromInt16
             val fInt32 = fromInt32
             val fInt64 = fromInt64
             val fIntInf = fromIntInf)
      in
         val fromLargeInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = rounding_mode -> real -> 'a
             val fInt8 = toInt8
             val fInt16 = toInt16
             val fInt32 = toInt32
             val fInt64 = toInt64
             val fIntInf = toIntInf)
      in
         val toInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = rounding_mode -> real -> 'a
             val fInt8 = toInt8
             val fInt16 = toInt16
             val fInt32 = toInt32
             val fInt64 = toInt64
             val fIntInf = toIntInf)
      in
         val toLargeInt = S.f
      end

      val floor = toInt TO_NEGINF
      val ceil = toInt TO_POSINF
      val trunc = toInt TO_ZERO
      val round = toInt TO_NEAREST

      local
         fun 'a make {fromWordUnsafe: 'a -> real,
                      toWordUnsafe: real -> 'a,
                      other : {maxWord': Word.word -> 'a,
                               wordSize: int,
                               zeroWord: 'a}} =
            (fromWordUnsafe,
             if Int.<= (precision, #wordSize other)
                then let
                        val trim = Int.- (#wordSize other, precision)
                        val maxWord' = (#maxWord' other) (Word.fromInt trim)
                        val maxWord = fromWordUnsafe maxWord'
                        val zeroWord = #zeroWord other
                     in
                        fn (m: rounding_mode) => fn x =>
                        case class x of
                           INF => raise Overflow
                         | NAN => raise Domain
                         | _ => if zero <= x
                                   then if x <= maxWord
                                           then safeConvert (m, toWordUnsafe, x)
                                        else raise Overflow
                                else if x > ~one
                                   then (case m of
                                            TO_NEGINF => raise Overflow
                                          | TO_POSINF => zeroWord
                                          | TO_ZERO => zeroWord
                                          | TO_NEAREST =>
                                               if x < ~half
                                                  then raise Overflow
                                               else zeroWord)
                                else raise Overflow
                     end
             else let
                     val maxWord' = (#maxWord' other) 0w0
                     val maxWord = fromWordUnsafe maxWord'
                     val zeroWord = #zeroWord other
                  in
                     fn (m: rounding_mode) => fn x =>
                     case class x of
                        INF => raise Overflow
                      | NAN => raise Domain
                      | _ => if zero <= x
                                then if x <= maxWord
                                        then safeConvert (m, toWordUnsafe, x)
                             else if x < maxWord + one
                                then (case m of
                                         TO_NEGINF => maxWord'
                                       | TO_POSINF => raise Overflow
                                       | TO_ZERO => maxWord'
                                       | TO_NEAREST =>
                                            (* Depends on maxWord being odd. *)
                                            if x - maxWord >= half
                                               then raise Overflow
                                            else maxWord')
                                else raise Overflow
                             else if x > ~one
                                then (case m of
                                         TO_NEGINF => raise Overflow
                                       | TO_POSINF => zeroWord
                                       | TO_ZERO => zeroWord
                                       | TO_NEAREST =>
                                            if x < ~half
                                               then raise Overflow
                                            else zeroWord)
                             else raise Overflow
                  end)
      in
         val (fromWord8,toWord8) =
            make {fromWordUnsafe = R.fromWord8Unsafe,
                  toWordUnsafe = R.toWord8Unsafe,
                  other = {maxWord' = fn w => Word8.<< (Word8.>> (Word8.maxWord', w), w),
                           wordSize = Word8.wordSize,
                           zeroWord = Word8.zero}}
         val (fromWord16,toWord16) =
            make {fromWordUnsafe = R.fromWord16Unsafe,
                  toWordUnsafe = R.toWord16Unsafe,
                  other = {maxWord' = fn w => Word16.<< (Word16.>> (Word16.maxWord', w), w),
                           wordSize = Word16.wordSize,
                           zeroWord = Word16.zero}}
         val (fromWord32,toWord32) =
            make {fromWordUnsafe = R.fromWord32Unsafe,
                  toWordUnsafe = R.toWord32Unsafe,
                  other = {maxWord' = fn w => Word32.<< (Word32.>> (Word32.maxWord', w), w),
                           wordSize = Word32.wordSize,
                           zeroWord = Word32.zero}}
         val (fromWord64,toWord64) =
            make {fromWordUnsafe = R.fromWord64Unsafe,
                  toWordUnsafe = R.toWord64Unsafe,
                  other = {maxWord' = fn w => Word64.<< (Word64.>> (Word64.maxWord', w), w),
                           wordSize = Word64.wordSize,
                           zeroWord = Word64.zero}}
      end

      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> real
             val fWord8 = fromWord8
             val fWord16 = fromWord16
             val fWord32 = fromWord32
             val fWord64 = fromWord64)
      in
         val fromWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> real
             val fWord8 = fromWord8
             val fWord16 = fromWord16
             val fWord32 = fromWord32
             val fWord64 = fromWord64)
      in
         val fromLargeWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = rounding_mode -> real -> 'a
             val fWord8 = toWord8
             val fWord16 = toWord16
             val fWord32 = toWord32
             val fWord64 = toWord64)
      in
         val toWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = rounding_mode -> real -> 'a
             val fWord8 = toWord8
             val fWord16 = toWord16
             val fWord32 = toWord32
             val fWord64 = toWord64)
      in
         val toLargeWord = S.f
      end

      structure Math =
         struct
            open Prim.Math

            (* Patch functions to handle out-of-range args.  Many C math
             * libraries do not do what the SML Basis Spec requires.
             *)

            local
               fun patch f x =
                  if x < ~one orelse x > one
                     then nan
                  else f x
            in
               val acos = patch acos
               val asin = patch asin
            end

            local
               fun patch f x = if x < zero then nan else f x
            in
               val ln = patch ln
               val log10 = patch log10
            end

            (* The x86 doesn't get exp right on infs. *)
            val exp =
               if MLton.Codegen.isX86
                  andalso let open MLton.Platform.Arch in host = X86 end
                  then (fn x =>
                        case class x of
                           INF => if x > zero then posInf else zero
                         | _ => exp x)
               else exp

            (* The Cygwin math library doesn't get pow right on some exceptional
             * cases.
             *
             * The Linux math library doesn't get pow (x, y) right when x < 0
             * and y is large (but finite).
             *
             * So, we define a pow function that gives the correct result on
             * exceptional cases, and only calls the C pow with x > 0.
             *)
            fun isInt (x: real): bool = x == realFloor x

            (* isEven x assumes isInt x. *)
            fun isEven (x: real): bool = isInt (x / two)

            fun isOddInt x = isInt x andalso not (isEven x)

            fun isNeg x = x < zero

            fun pow (x, y) =
               case class y of
                  INF =>
                     if class x = NAN
                        then nan
                     else if x < negOne orelse x > one
                        then if isNeg y then zero else posInf
                     else if negOne < x andalso x < one
                        then if isNeg y then posInf else zero
                     else (* x = 1 orelse x = ~1 *)
                        nan
                | NAN => nan
                | ZERO => one
                | _ =>
                     (case class x of
                         INF =>
                            if isNeg x
                               then if isNeg y
                                       then if isOddInt y
                                               then negZero
                                            else zero
                                    else if isOddInt y
                                            then negInf
                                         else posInf
                            else (* x = posInf *)
                               if isNeg y then zero else posInf
                       | NAN => nan
                       | ZERO =>
                            if isNeg y
                               then if isOddInt y
                                       then copySign (posInf, x)
                                    else posInf
                            else if isOddInt y
                                    then x
                                 else zero
                       | _ =>
                            if isNeg x
                               then if isInt y
                                       then if isEven y
                                               then Prim.Math.pow (~ x, y)
                                            else negOne * Prim.Math.pow (~ x, y)
                                    else nan
                            else Prim.Math.pow (x, y))

            fun cosh x =
               case class x of
                  INF => x
                | ZERO => one
                | _ => R.Math.cosh x

            fun sinh x =
               case class x of
                  INF => x
                | ZERO => x
                | _ => R.Math.sinh x

            fun tanh x =
               case class x of
                  INF => if x > zero then one else negOne
                | ZERO => x
                | _ => R.Math.tanh x
         end
   end

structure Real32 = Real (structure W = Word32
                         structure R =
                            struct
                               open Primitive.Real32
                               local open Primitive.PackReal32 in
                                  val castToWord = castToWord
                                  val castFromWord = castFromWord
                               end
                            end)
structure Real64 = Real (structure W = Word64
                         structure R =
                            struct
                               open Primitive.Real64
                               local open Primitive.PackReal64 in
                                  val castToWord = castToWord
                                  val castFromWord = castFromWord
                               end
                            end)
