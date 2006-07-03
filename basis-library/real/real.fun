(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Real (R: PRE_REAL): REAL =
   struct
      structure MLton = Primitive.MLton
      structure Prim = R
      local
         open IEEEReal
      in
         datatype z = datatype float_class
         datatype rounding_mode = datatype rounding_mode
      end
      infix 4 == != ?=
      type real = Prim.real

      local
         open Prim
         val isBytecode = MLton.Codegen.isBytecode
      in
         val *+ =
            if isBytecode
               then fn (r1, r2, r3) => r1 * r2 + r3
            else *+
         val *- =
            if isBytecode
               then fn (r1, r2, r3) => r1 * r2 - r3
            else *-
         val op * = op *
         val op + = op +
         val op - = op -
         val op / = op /
         val op / = op /
         val op < = op <
         val op <= = op <=
         val op > = op >
         val op >= = op >=
         val ~ = ~
         val abs = abs
         val fromInt = fromInt
         val fromLarge = fromLarge
         val maxFinite = maxFinite
         val minNormalPos = minNormalPos
         val minPos = minPos
         val precision = precision
         val radix = radix
         val signBit = fn r => signBit r <> 0
         val toLarge = toLarge
      end

      val zero = fromLarge TO_NEAREST 0.0
      val one = fromLarge TO_NEAREST 1.0
      val negOne = ~ one
      val two = fromLarge TO_NEAREST 2.0
      val half = one / two

      val posInf = one / zero
      val negInf = ~one / zero

      val nan = posInf + negInf

      structure Class = Primitive.Real64.Class
      local
         val classes =
            let
               open Class
            in
               (* order here is chosen based on putting the more commonly used
                * classes at the front.
                *)
               [(normal, NORMAL),
                (zero, ZERO),
                (inf, INF),
                (nan, NAN),
                (subnormal, SUBNORMAL)]
            end
      in
         fun class x =
            let
               val i = Prim.class x
            in
               case List.find (fn (i', _) => i = i') classes of
                  NONE => raise Fail "Real_class returned bogus integer"
                | SOME (_, c) => c
            end
      end
   
      val abs =
         if MLton.Codegen.isNative
            then abs
         else
            fn x =>
            case class x of
               INF => posInf
             | NAN => x
             | _ => if signBit x then ~x else x
         
      fun isFinite r =
         case class r of
            INF => false
          | NAN => false
          | _ => true

      val op == = Prim.==

      val op != = not o op ==

      fun isNan r = r != r

      fun isNormal r = class r = NORMAL

      val op ?= =
         if MLton.Codegen.isNative
            then Prim.?=
         else
            fn (x, y) =>
            case (class x, class y) of
               (NAN, _) => true
             | (_, NAN) => true
             | (ZERO, ZERO) => true
             | _ => Prim.== (x, y)

      fun min (x, y) =
         if isNan x
            then y
         else if isNan y
                 then x
              else if x < y then x else y

      fun max (x, y) =
         if isNan x
            then y
         else if isNan y
                 then x
              else if x > y then x else y

      fun sign (x: real): int =
         case class x of
            NAN => raise Domain
          | ZERO => 0
          | _ => if x > zero then 1 else ~1

      fun sameSign (x, y) = signBit x = signBit y

      fun copySign (x, y) =
         if sameSign (x, y)
            then x
         else ~ x

      local
         datatype z = datatype IEEEReal.real_order
      in
         fun compareReal (x, y) =
            case (class x, class y) of
               (NAN, _) => UNORDERED
             | (_, NAN) => UNORDERED
             | (ZERO, ZERO) => EQUAL
             | _ => if x < y then LESS
                    else if x > y then GREATER
                         else EQUAL
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

      val nextAfter: real * real -> real =
         fn (r, t) =>
         case (class r, class t) of
            (NAN, _) => nan
          | (_, NAN) => nan
          | (INF, _) => r
          | (ZERO, ZERO) => r
          | (ZERO, _) => if t > zero then minPos else ~minPos
          | _ =>
               if r == t
                  then r
               else
                  let
                     fun doit (r, t) =
                        if r == maxFinite andalso t == posInf
                           then posInf
                        else if r > t
                                then R.nextAfterDown r
                             else R.nextAfterUp r
                  in
                     if r > zero
                        then doit (r, t)
                     else ~ (doit (~r, ~t))
                  end
                         
      val toManExp =
         let
            val r: int ref = ref 0
         in
            fn x =>
            case class x of
               INF => {exp = 0, man = x}
             | NAN => {exp = 0, man = nan}
             | ZERO => {exp = 0, man = x}
             | _ => 
                  let
                     val man = Prim.frexp (x, r)
                  in
                     {exp = !r, man = man}
                  end
         end

      fun fromManExp {exp, man} = Prim.ldexp (man, exp)

      val fromManExp =
         if MLton.Codegen.isNative
            then fromManExp
         else
            fn {exp, man} =>
            case class man of
               INF => man
             | NAN => man
             | ZERO => man
             | _ => fromManExp {exp = exp, man = man}

      local
         val int = ref zero
      in
         fun split x =
            case class x of
               INF => {frac = if x > zero then zero else ~zero,
                       whole = x}
             | NAN => {frac = nan, whole = nan}
             | _ => 
                  let
                     val frac = Prim.modf (x, int)
                     val whole = !int
                     (* Some platforms' C libraries don't get sign of zero right.
                      *)
                     fun fix y =
                        if class y = ZERO
                           andalso not (sameSign (x, y))
                           then ~ y
                        else y
                  in
                     {frac = fix frac,
                      whole = fix whole}
                  end
      end

      val realMod = #frac o split
         
      fun checkFloat x =
         case class x of
            INF => raise Overflow
          | NAN => raise Div
          | _ => x

      val maxInt = fromInt Int.maxInt'
      val minInt = fromInt Int.minInt'

      fun roundReal (x: real, m: rounding_mode): real =
         fromLarge
         TO_NEAREST
         (IEEEReal.withRoundingMode (m, fn () =>
                                     (Primitive.Real64.round (toLarge x))))
         
      fun toInt mode x =
         case class x of
            INF => raise Overflow
          | NAN => raise Domain
          | _ =>
               if minInt <= x
                  then if x <= maxInt
                          then Prim.toInt (roundReal (x, mode))
                       else if x < maxInt + one
                               then (case mode of
                                        TO_NEGINF => Int.maxInt'
                                      | TO_POSINF => raise Overflow
                                      | TO_ZERO => Int.maxInt'
                                      | TO_NEAREST =>
                                           (* Depends on maxInt being odd. *)
                                           if x - maxInt >= half
                                              then raise Overflow
                                           else Int.maxInt')
                            else raise Overflow
               else if x > minInt - one
                       then (case mode of
                                TO_NEGINF => raise Overflow
                              | TO_POSINF => Int.minInt'
                              | TO_ZERO => Int.minInt'
                              | TO_NEAREST =>
                                   (* Depends on minInt being even. *)
                                   if x - minInt < ~half
                                      then raise Overflow
                                   else Int.minInt')
                    else raise Overflow
      
      val floor = toInt TO_NEGINF
      val ceil = toInt TO_POSINF
      val trunc = toInt TO_ZERO
      val round = toInt TO_NEAREST

      local
         fun round mode x =
            case class x of
               INF => x
             | NAN => x
             | _ => roundReal (x, mode)
      in
         val realCeil = round TO_POSINF
         val realFloor = round TO_NEGINF
         val realRound = round TO_NEAREST
         val realTrunc = round TO_ZERO
      end

      fun rem (x, y) =
         case class x of
            INF => nan
          | NAN => nan
          | ZERO => zero
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
(*                val x = concat ["0.", digits, "E", exp, "\000"] *)
                  val n  =
                     Int.+ (4, Int.+ (List.length digits, String.size exp))
                  val a = Array.rawArray n
                  fun up (i, c) = (Array.update (a, i, c); Int.+ (i, 1))
                  val i = 0
                  val i = up (i, #"0")
                  val i = up (i, #".")
                  val i =
                     List.foldl
                     (fn (d, i) =>
                      if Int.< (d, 0) orelse Int.> (d, 9)
                         then raise Bad
                      else up (i, Char.chr (Int.+ (d, Char.ord #"0"))))
                     i digits
                  val i = up (i, #"E")
                  val i = CharVector.foldl (fn (c, i) => up (i, c)) i exp
                  val _ = up (i, #"\000")
                  val x = Vector.fromArray a
                  val x = Prim.strto (NullString.fromString x)
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
                   | ZERO => if sign then ~ zero else zero)
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
            INF => {class = INF,
                    digits = [],
                    exp = 0,
                    sign = x < zero}
          | NAN => {class = NAN,
                    digits = [],
                    exp = 0,
                    sign = false}
          | ZERO => {class = ZERO,
                     digits = [],
                     exp = 0,
                     sign = signBit x}
          | c => 
               let
                  val (cs, exp) = gdtoa (x, Gen, 0)
                  fun loop (i, ac) =
                     if Int.< (i, 0)
                        then ac
                     else loop (Int.- (i, 1),
                                (Int.- (Char.ord (C.CS.sub (cs, i)),
                                        Char.ord #"0"))
                                :: ac)
                  val digits = loop (Int.- (C.CS.length cs, 1), [])
               in
                  {class = c,
                   digits = digits,
                   exp = exp,
                   sign = x < zero}
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
         fun sci (x: real, ndig: int): string =
            let
               val sign = if x < zero then "~" else ""
               val (cs, decpt) = gdtoa (x, Sci, add1 ndig)
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
         fun gen (x: real, n: int): string =
            case class x of
               INF => if x > zero then "inf" else "~inf"
             | NAN => "nan"
             | _ => 
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
                                    if Primitive.safe andalso Int.< (n, 0)
                                       then raise Size
                                    else n
                        in
                           fn x =>
                           let
                              val sign = if x < zero then "~" else ""
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
                           fn x => gen (x, n)
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
                           fn x => sci (x, n)
                        end
            in
               fn x =>
               case class x of
                  NAN => "nan"
                | INF => if x > zero then "inf" else "~inf"
                | _ => doit x
            end
      end
   
      val toString = fmt (StringCvt.GEN NONE)

      val fromLargeInt: LargeInt.int -> real =
         fn i =>
         fromInt (IntInf.toInt i)
         handle Overflow =>
            let
               val (i, sign) =
                  if LargeInt.< (i, 0)
                     then (LargeInt.~ i, true)
                  else (i, false)
               val x = Prim.strto (NullString.fromString
                                   (concat [LargeInt.toString i, "\000"]))
            in
               if sign then ~ x else x             
            end
         
      val toLargeInt: IEEEReal.rounding_mode -> real -> LargeInt.int =
         fn mode => fn x =>
         case class x of
            INF => raise Overflow
          | NAN => raise Domain
          | ZERO => 0
          | _ =>
               let
                  (* This round may turn x into an INF, so we need to check the
                   * class again.
                   *)
                  val x = roundReal (x, mode)
               in
                  case class x of
                     INF => raise Overflow
                   | _ => 
                        if minInt <= x andalso x <= maxInt
                           then LargeInt.fromInt (Prim.toInt x)
                        else
                           valOf
                           (LargeInt.fromString (fmt (StringCvt.FIX (SOME 0)) x))
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
               if MLton.Codegen.isNative
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
                                               then ~ zero
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
