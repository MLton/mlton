functor Test (structure Real: REAL
              val size: int) =
struct

open Real
open Math

infix == !=

datatype z = datatype IEEEReal.float_class
datatype z = datatype IEEEReal.rounding_mode
datatype z = datatype General.order
   
val b2s = Bool.toString
val i2s = Int.toString
val exact = fmt StringCvt.EXACT

val s2r = valOf o fromString
   
val zero = s2r "0.0"
val one = s2r "1.0"
val two = s2r "2.0"
val nan = posInf + negInf

val halfMaxFinite = maxFinite / two
val halfMinNormalPos = minNormalPos / two
   
val reals =
   [maxFinite,
    halfMaxFinite,
    s2r "1.23E3",
    s2r "1.23E1",
    Math.pi,
    Math.e,
    s2r "1.23E0",
    s2r "1.23E~1",
    s2r "1.23E~3",
    minNormalPos,
    halfMinNormalPos,
    minPos,
    zero]

fun for f = (List.app f reals; List.app (f o ~) reals)

val reals' =
   [posInf,
    negInf,
    posInf + negInf,
    maxFinite * s2r "2.0"]

fun for' f = (for f; List.app f reals')

val _ = print (concat ["\nTesting Real", Int.toString size, "\n"])

val _ = print "\nTesting fmt\n"

val _ =
   for
   (fn r =>
    List.app (fn spec => print (concat [fmt spec r, "\n"]))
    let
       open StringCvt
    in
       [EXACT, SCI NONE, FIX NONE, GEN NONE,
        SCI (SOME 0), FIX (SOME 0), GEN (SOME 1),
        SCI (SOME 10), FIX (SOME 10), GEN (SOME 10)]
    end)

val _ =
   let
      fun doit (s,r, s0, s1, s2, s6) =
         if (fmt (StringCvt.FIX (SOME 0)) r = s0
             andalso fmt (StringCvt.FIX (SOME 1)) r = s1
             andalso fmt (StringCvt.FIX (SOME 2)) r = s2
             andalso fmt (StringCvt.FIX (SOME 6)) r = s6
             andalso fmt (StringCvt.FIX NONE) r = s6)
            then ()
         else raise Fail (concat ["fmt bug: ", s, " ", exact r])
   in
      List.app
      (fn (s,r, s0, s1, s2, s6) =>
       (doit (s,r, s0, s1, s2, s6)
        ; if r == zero
             then ()
          else doit (s^"~",~r, "~"^s0, "~"^s1, "~"^s2, "~"^s6)))
      [("a", s2r "0.0", "0", "0.0", "0.00", "0.000000"),
       ("b", s2r "1.0", "1", "1.0", "1.00", "1.000000"),
       ("c", s2r "1.4", "1", "1.4", "1.40", "1.400000"),
       ("d", s2r "1.5", "2", "1.5", "1.50", "1.500000"),
       ("e", s2r "2.5", "2", "2.5", "2.50", "2.500000"),
       ("f", s2r "1.6", "2", "1.6", "1.60", "1.600000"),
       ("h", s2r "3.141592653589", "3", "3.1", "3.14", "3.141593"),
       ("j", s2r "91827365478400.0", "91827365478400", "91827365478400.0", 
        "91827365478400.00", "91827365478400.000000")]
   end

val _ =
   let
      fun chkSCI (r, s0, s1, s2, s6) = 
         fmt (StringCvt.SCI (SOME 0)) r = s0
         andalso fmt (StringCvt.SCI (SOME 1)) r = s1
         andalso fmt (StringCvt.SCI (SOME 2)) r = s2
         andalso fmt (StringCvt.SCI (SOME 6)) r = s6
         andalso fmt (StringCvt.SCI NONE) r = s6
   in
      List.app
      (fn (r, s0, s1, s2, s6) =>
       if chkSCI(r, s0, s1, s2, s6) 
          andalso (r == zero orelse chkSCI(~r, "~"^s0, "~"^s1, "~"^s2, "~"^s6))
          then ()
       else raise Fail (concat ["fmt SCI bug: ", exact r]))
      [(s2r "0.0", "0E0", "0.0E0", "0.00E0", "0.000000E0"),
       (s2r "0.0012345678", "1E~3", "1.2E~3", "1.23E~3", "1.234568E~3"),
       (s2r "1.0", "1E0", "1.0E0", "1.00E0", "1.000000E0"),
       (s2r "1.4", "1E0", "1.4E0", "1.40E0", "1.400000E0"),
       (s2r "1.5", "2E0", "1.5E0", "1.50E0", "1.500000E0"),
       (s2r "1.6", "2E0", "1.6E0", "1.60E0", "1.600000E0"),
       (s2r "3.141592653589", "3E0", "3.1E0", "3.14E0", "3.141593E0"),
       (s2r "91827365478400.0", "9E13", "9.2E13", "9.18E13", "9.182737E13")]
   end

val _ =
   let
      fun chkGEN (r, s1, s2, s6, s12) = 
         fmt (StringCvt.GEN (SOME 1)) r = s1
         andalso fmt (StringCvt.GEN (SOME 2)) r = s2
         andalso fmt (StringCvt.GEN (SOME 6)) r = s6
         andalso fmt (StringCvt.GEN (SOME 12)) r = s12
         andalso fmt (StringCvt.GEN NONE) r = s12
         andalso toString r = s12;
   in
      List.app
      (fn (r, s1, s2, s6, s12) =>
       if chkGEN(r, s1, s2, s6, s12) 
          andalso (r == zero orelse 
                   chkGEN(~r, "~"^s1, "~"^s2, "~"^s6, "~"^s12))
          then ()
       else raise Fail (concat ["fmt GEN bug: ", exact r]))
      [(s2r "0.0",               "0", "0",     "0", "0"),
       (s2r "1.0",              "1", "1",  "1", "1"),
       (s2r "1.5",              "2", "1.5",  "1.5", "1.5"),
       (s2r "91827365478400.0", "9E13", "9.2E13",  "9.18274E13", 
        "91827365478400")]
   end

val _ = print "\nTesting scan\n"

val _ = for' (fn r =>
              let
                 val r' = valOf (StringCvt.scanString scan (exact r))
                 val _ = print (concat [exact r, "\t", exact r', "\n"])
              in
                 if r == r' orelse unordered (r, r')
                    then ()
                 else raise Fail "scan bug"
              end)

val _ = print "\nTesting checkFloat\n"
val _ =
   for'
   (fn r =>
    if (case class r of
           INF => ((checkFloat r; false) handle Overflow => true | _ => false)
         | NAN => ((checkFloat r; false) handle Div => true | _ => false)
         | _ => (checkFloat r; true) handle _ => false)
       then ()
    else raise Fail "checkFloat bug")
   
val _ = print "\nTesting class, isFinite, isNan, isNormal\n"
val _ =
   for'
   (fn r =>
    let
       val c = 
          case class r of
             INF => "inf"
           | NAN => "nan"
           | NORMAL => "normal"
           | SUBNORMAL => "subnormal"
           | ZERO => "zero"
    in
       print (concat [exact r, "\t", c, "\n",
                      "\tisFinite = ", b2s (isFinite r),
                      "\tisNan = ", b2s (isNan r),
                      "\tisNormal = ", b2s (isNormal r),
                      "\n"])
    end)

val _ = print "\nTesting maxFinite, minPos, minNormalPos\n"

local
  val isNormal = Real.isNormal
  val isFinite = Real.isFinite
  val isPositive = fn r =>
     case class r of
        NORMAL => r > zero
      | SUBNORMAL => r > zero
      | INF => r > zero
      | _ => false

  fun min (p: real -> bool): real =
    let
      fun loop (x: real): real =
        let
          val y = x / two
        in
          if p y
            then loop y
            else x
        end
    in
       loop one
    end
in
  val minNormalPos = min isNormal
  val minPos = min isPositive
    
  val maxFinite =
    let
      fun up (x: real): real =
        let
          val y = x * two
        in
          if isFinite y
            then up y
            else x
        end
      fun down (x: real, y: real): real =
                  let
                    val y = y / two
                    val z = x + y
                  in
                    if isFinite z
                      then down (z, y)
                      else x
                  end
      val z = up one
    in
       down (z, z)
    end
end

val _ = print ((Real.toString maxFinite) ^ "\n")
val _ = print ((Real.toString Real.maxFinite) ^ "\n")
val _ = print ((Bool.toString (Real.==(Real.maxFinite, maxFinite))) ^ "\n")
val _ = print ((Real.toString minPos) ^ "\n")
val _ = print ((Real.toString Real.minPos) ^ "\n")
val _ = print ((Bool.toString (Real.==(Real.minPos, minPos))) ^ "\n")
val _ = print ((Real.toString minNormalPos) ^ "\n")
val _ = print ((Real.toString Real.minNormalPos) ^ "\n")
val _ = print ((Bool.toString (Real.==(Real.minNormalPos, minNormalPos))) ^ "\n")

val _ = print "\nTestring fromString\n"

val _ =
   List.app
   (fn (s1, s2) =>
    if valOf (fromString s1) == valOf (fromString s2)
       then ()
    else raise Fail "fromString bug")
   [("12.", "12.0"),
    ("12.E", "12.0"),
    ("12.E+", "12.0"),
    ("12.E-", "12.0"),
    ("12.E2", "12.0"),
    ("12.E+2", "12.0"),
    ("12.E-2", "12.0"),
    ("12E+", "12.0"),
    ("12E-", "12.0"),
    ("0", "0.0"),
    ("156", "156.0"),
    ("+156", "156.0"), 
    ("~156", "~156.0"), 
    ("-156", "~156.0"), 
    ("156.25", "156.25"), 
    ("+156.25", "156.25"), 
    ("~156.25", "~156.25"), 
    ("-156.25", "~156.25"),
    (".25", "0.25"),
    ("+.25", "0.25"),
    ("~.25", "~0.25"),
    ("-.25", "~0.25"),
    ("156E024", "156E024"),
    ("+156E024", "156E024"),
    ("~156E024", "~156E024"),
    ("-156E024", "~156E024"),
    ("156.25E024", "156.25E024"),
    ("+156.25E024", "156.25E024"),
    ("~156.25E024", "~156.25E024"),
    ("-156.25E024", "~156.25E024"),
    (".25E024", "0.25E024"),
    ("+.25E024", "0.25E024"),
    ("~.25E024", "~0.25E024"),
    ("-.25E024", "~0.25E024"),
    ("156E+024", "156E024"),
    ("+156E+024", "156E024"),
    ("~156E+024", "~156E024"),
    ("-156E+024", "~156E024"),
    ("156.25E+024", "156.25E024"),
    ("+156.25E+024", "156.25E024"),
    ("~156.25E+024", "~156.25E024"),
    ("-156.25E+024", "~156.25E024"),
    (".25E+024", "0.25E024"),
    ("+.25E+024", "0.25E024"),
    ("~.25E+024", "~0.25E024"),
    ("-.25E+024", "~0.25E024"),
    ("156E~024", "156E~024"),
    ("+156E~024", "156E~024"),
    ("~156E~024", "~156E~024"),
    ("-156E~024", "~156E~024"),
    ("156.25E~024", "156.25E~024"),
    ("+156.25E~024", "156.25E~024"),
    ("~156.25E~024", "~156.25E~024"),
    ("-156.25E~024", "~156.25E~024"),
    (".25E~024", "0.25E~024"),
    ("+.25E~024", "0.25E~024"),
    ("~.25E~024", "~0.25E~024"),
    ("-.25E~024", "~0.25E~024"),
    ("156E-024", "156E~024"),
    ("+156E-024", "156E~024"),
    ("~156E-024", "~156E~024"),
    ("-156E-024", "~156E~024"),
    ("156.25E-024", "156.25E~024"),
    ("+156.25E-024", "156.25E~024"),
    ("~156.25E-024", "~156.25E~024"),
    ("-156.25E-024", "~156.25E~024"),
    (".25E-024", "0.25E~024"),
    ("+.25E-024", "0.25E~024"),
    ("~.25E-024", "~0.25E~024"),
    ("-.25E-024", "~0.25E~024"),
    ("156e024", "156E024"),
    ("+156e024", "156E024"),
    ("~156e024", "~156E024"),
    ("-156e024", "~156E024"),
    ("156.25e024", "156.25E024"),
    ("+156.25e024", "156.25E024"),
    ("~156.25e024", "~156.25E024"),
    ("-156.25e024", "~156.25E024"),
    (".25e024", "0.25E024"),
    ("+.25e024", "0.25E024"),
    ("~.25e024", "~0.25E024"),
    ("-.25e024", "~0.25E024"),
    ("156e+024", "156E024"),
    ("+156e+024", "156E024"),
    ("~156e+024", "~156E024"),
    ("-156e+024", "~156E024"),
    ("156.25e+024", "156.25E024"),
    ("+156.25e+024", "156.25E024"),
    ("~156.25e+024", "~156.25E024"),
    ("-156.25e+024", "~156.25E024"),
    (".25e+024", "0.25E024"),
    ("+.25e+024", "0.25E024"),
    ("~.25e+024", "~0.25E024"),
    ("-.25e+024", "~0.25E024"),
    ("156e~024", "156E~024"),
    ("+156e~024", "156E~024"),
    ("~156e~024", "~156E~024"),
    ("-156e~024", "~156E~024"),
    ("156.25e~024", "156.25E~024"),
    ("+156.25e~024", "156.25E~024"),
    ("~156.25e~024", "~156.25E~024"),
    ("-156.25e~024", "~156.25E~024"),
    (".25e~024", "0.25E~024"),
    ("+.25e~024", "0.25E~024"),
    ("~.25e~024", "~0.25E~024"),
    ("-.25e~024", "~0.25E~024"),
    ("156e-024", "156E~024"),
    ("+156e-024", "156E~024"),
    ("~156e-024", "~156E~024"),
    ("-156e-024", "~156E~024"),
    ("156.25e-024", "156.25E~024"),
    ("+156.25e-024", "156.25E~024"),
    ("~156.25e-024", "~156.25E~024"),
    ("-156.25e-024", "~156.25E~024"),
    (".25e-024", "0.25E~024"),
    ("+.25e-024", "0.25E~024"),
    ("~.25e-024", "~0.25E~024"),
    ("-.25e-024", "~0.25E~024")]


val _ = print "\nTesting {from,to}Decimal\n"
   
val _ =
   List.app (fn r =>
             let
                val da = valOf (IEEEReal.fromString r)
                val s1 = IEEEReal.toString da
                val x = valOf (fromDecimal da)
                val s2 = exact x
                val da' = toDecimal x
                val b = Bool.toString (da = da')
             in
                print (concat [s1, " ", s2, " ", b, "\n"])
             end)
   ["inf", "+inF", "~iNf", "-Inf",
    "infinity", "+infinity", "~infinity", "-infinity",
    "nan", "+naN", "~nAn", "-Nan",
    "0", "0.0", "0.0E0", "~0",
    "15",
    "1.5",
    "~1.5e+1",
    "15.0",
    ".15e~2",
    ".15e-2",
    "000.0015e0",
    "1.2E999",
    "~1.2E999",
    "1E~999",
    "~1E~999",
    "1E12345678901234567890"]

val _ = print "\nTesting {from,to}LargeInt\n"
val _ =
   for
   (fn r =>
    let
       val i = toLargeInt IEEEReal.TO_NEGINF r
       val r' = fromLargeInt i
       val _ = print (concat [exact r,
                              "\t", LargeInt.toString i,
                              "\t", exact r',
                              "\n"])
    in
       if r' == realFloor r
          then ()
       else raise Fail "bug"
    end)

val roundingModes =
   [(TO_NEAREST, "nearest"),
    (TO_NEGINF, "neginf"),
    (TO_POSINF, "posinf"),
    (TO_ZERO, "zero")]

val _ =
   let
      fun doit (x, mode, name) =
         let
            val i = toLargeInt mode x
         in
            print (concat [name, "\t", exact x, "\t", LargeInt.toString i, "\n"])
         end
   in
      List.app
      (fn (mode, name) =>
       List.app (fn s =>
                 let
                    val x = s2r s
                 in
                    doit (x, mode, name)
                    ; doit (~ x, mode, name)
                    ; doit (s2r "1E12" + x, mode, name)
                    ; doit (s2r "~1E12" + x, mode, name)
                 end)
       ["0.0", "0.25", "0.5", "0.75", "1.0", "1.25", "1.5", "1.75", "2.0",
        "2.5", "3.0"])
      roundingModes
   end

val _ = print "\nTesting fromInt\n"

val _ =
   for
   (fn r =>
    case SOME (round r) handle Overflow => NONE of
       NONE => ()
     | SOME i =>
          let
             val r = fromInt i
          in
             if r == fromInt (round r)
                then ()
             else raise Fail "fromInt bug"
          end)

val _ = print "\nTesting toInt\n"

val _ =
   for
   (fn r =>
    List.app
    (fn (mode, name) =>
     case SOME (toInt mode r) handle Overflow => NONE of
        NONE => ()
      | SOME i => if i = LargeInt.toInt (toLargeInt mode r)
                     then ()
                  else raise Fail "bug")
    roundingModes)

val _ = print "\nTesting ceil,floor,round,trunc\n"

val _ =
   for
   (fn r =>
    List.app
    (fn (mode, f) =>
     case SOME (toInt mode r) handle Overflow => NONE of
        NONE => ()
      | SOME i => if i = f r
                     then ()
                  else raise Fail "bug")
    [(TO_NEAREST, round),
     (TO_NEGINF, floor),
     (TO_POSINF, ceil),
     (TO_ZERO, trunc)])

val _ = print "\nTesting copySign, sameSign, sign, signBit\n"
val _ =
    for'
    (fn r1 =>
     (for'
      (fn r2 =>
       if unordered (r1, r2)
          orelse (if false
                     then print (concat [b2s (signBit r1), "\t",
                                         b2s (signBit r2), "\t",
                                         i2s (sign r1), "\t",
                                         b2s (sameSign (r1, r2)), "\t",
                                         exact (copySign (r1, r2)), "\n"])
                  else ()
                     ; (signBit r1 = Int.< (sign r1, 0)
                        orelse r1 == zero)
                     andalso (sameSign (r1, r2)) = (signBit r1 = signBit r2)
                     andalso sameSign (r2, copySign (r1, r2)))
          then ()
       else raise Fail "bug")))

val _ = print "\nTesting max, min\n"

val _ =
   for'
   (fn r1 =>
    for'
    (fn r2 =>
     let
        val max = max (r1, r2)
        val min = min (r1, r2)
     in
        if (isNan r1 orelse (r1 <= max andalso min <= r1))
           andalso (isNan r2 orelse (r2 <= max andalso min <= r2))
           andalso (r1 == max orelse r2 == max
                    orelse (isNan r1 andalso isNan r2))
           andalso (r1 == min orelse r2 == min
                    orelse (isNan r1 andalso isNan r2))
           then ()
        else raise Fail "bug"
     end))

val _ = print "\nTesting Real.Math.{acos,asin,atan,cos,cosh,exp,ln,log10,sin,sinh,sqrt,tan,tanh}\n"
   
val _ =
   for' (fn r =>
         List.app
         (fn (name, f, except) =>
          if List.exists (fn r' => r == r') except
             then ()
          else
             print (concat [(*name, " ", exact r, " = ", *)
                            fmt (StringCvt.GEN (SOME 10)) (f r), "\n"]))
         let
            open Real.Math
         in
            [("acos", acos, []),
             ("asin", asin, []),
             ("atan", atan, []),
             ("cos", cos, [maxFinite, halfMaxFinite,
                           ~maxFinite, ~halfMaxFinite]),
             ("cosh", cosh, [s2r "12.3", s2r "~12.3", e, ~e]),
             ("exp", exp, [s2r "12.3", pi, s2r "1.23",
                           s2r "~12.3", ~pi, s2r "~1.23"]),
             ("ln", ln, []),
             ("log10", log10, [s2r "1.23", pi]),
             ("sin", sin, [maxFinite, halfMaxFinite,
                           ~maxFinite, ~halfMaxFinite, pi, ~pi]),
             ("sinh", sinh, [pi, ~pi, s2r "0.123", s2r "~0.123"]),
             ("sqrt", sqrt, [maxFinite]),
             ("tan", tan, [maxFinite, halfMaxFinite,
                           ~maxFinite, ~halfMaxFinite, pi, ~pi]),
             ("tanh", tanh, [s2r "0.123", s2r "~0.123"])]
         end)

val _ = print "\nTesting Real.{*,+,-,/,nextAfter,rem} Real.Math.{atan2,pow}\n"
val _ =
   for'
   (fn r1 =>
    for'
    (fn r2 =>
     List.app
     (fn (name, f, except) =>
      if List.exists (fn (r1', r2') => r1 == r1' andalso r2 == r2') except
         then ()
      else
         print (concat [(*name, " (", exact r1, ", ", exact r2, ") = ", *)
                        exact (f (r1, r2)), "\n"]))
     [("*", op *, []),
      ("+", op +, []),
      ("-", op -, []),
      ("/", op /, [(s2r "1.23", halfMaxFinite),
                   (s2r "1.23", ~halfMaxFinite),
                   (s2r "~1.23", halfMaxFinite),
                   (s2r "~1.23", ~halfMaxFinite)
                   ]),
      ("nextAfter", nextAfter, [])
(*      ("rem", rem, []), *)
(*      ("atan2", Math.atan2, []), *)
(*      ("pow", Math.pow, [(halfMaxFinite, s2r "0.123"), (pi, e)]) *)
      ]))

val _ =
   if List.all (op ==) [(posInf + posInf, posInf),
                        (negInf + negInf, negInf),
                        (posInf - negInf, posInf),
                        (negInf - posInf, negInf)]
      andalso List.all isNan [nan, nan + one, nan - one, nan * one, nan / one]
      andalso List.all isNan [posInf + negInf, negInf + posInf, posInf - posInf,
                              negInf - negInf]
      then ()
   else raise Fail "bug"

val _ = print "\nTesting *+, *-\n"
val _ =
   for
   (fn r1 =>
    for
    (fn r2 =>
     for
     (fn r3 =>
      if *+ (r1, r2, r3) == r1 * r2 + r3
         then ()
      else raise Fail "*+ bug")))

val _ = print "\nTesting Real.{realCeil,realFloor,realTrunc}\n"
val _ =
   for
   (fn r =>
    let
       val ceil = realCeil r
       val floor = realFloor r
       val trunc = realTrunc r
       val _ = print (concat [exact r, "  ",
                              exact ceil, " ",
                              exact floor, " ",
                              exact trunc, "\n"])
    in
       if floor <= r
          andalso r <= ceil
          andalso abs trunc <= abs r
          then ()
       else raise Fail "bug"
    end)

val _ = print "\nTesting Real.{<,<=,>,>=,==,!=,?=,unordered}\n"

val _ =
   for
   (fn r1 =>
    for
    (fn r2 =>
     let
        val _ = 
           List.app
           (fn (f, name) =>
            print (concat [(* name, " (", exact r1, ", ", exact r2, ") = ", *)
                           b2s (f (r1, r2)), "\n"]))
           [(Real.<, "<"),
            (Real.>, ">"),
            (Real.==, "=="),
            (Real.?=, "?=")]
     in
        if unordered (r1, r2) = (isNan r1 orelse isNan r2)
           andalso (r1 != r2) = not (r1 == r2)
           andalso if unordered (r1, r2)
                      then (false = (r1 <= r2)
                            andalso false = (r1 < r2)
                            andalso false = (r1 >= r2)
                            andalso false = (r1 > r2)
                            andalso false = (r1 == r2)
                            andalso if isNan r1 andalso isNan r2
                                       then true = ?= (r1, r2) 
                                    else true)
                   else ((r1 < r2) = not (r1 >= r2)
                         andalso (r1 > r2) = not (r1 <= r2))
           then ()
        else raise Fail "bug"
     end))

val _ = print "\nTesting compare, compareReal\n"

val _ =
   for
   (fn r =>
    for
    (fn r' =>
     let
        val c =
           case SOME (compare (r, r')) handle IEEEReal.Unordered => NONE of
              NONE => "Unordered"
            | SOME z =>
                 case z of
                    EQUAL => "EQUAL"
                  | GREATER => "GREATER"
                  | LESS => "LESS"
        datatype z = datatype IEEEReal.real_order
        val cr =
           case compareReal (r, r') of
              EQUAL => "EQUAL"
            | GREATER => "GREATER"
            | LESS => "LESS"
            | UNORDERED => "UNORDERED"
        val _ =
           print (concat [(* exact r, " ", exact r', "\t", *)
                          c, "\t", cr, "\n"])
     in
        if compareReal (r, r') = (case compareReal (r', r) of
                                     EQUAL => EQUAL
                                   | GREATER => LESS
                                   | LESS => GREATER
                                   | UNORDERED => UNORDERED)
           then ()
        else raise Fail "compareReal bug"
     end))

val _ = print "\nTesting abs\n"

val _ = for (fn r =>
             if abs r == abs (~ r)
                then ()
             else raise Fail "abs bug")

val _ = print "\nTesting {from,to}ManExp\n"
         
val _ =
   for
   (fn x =>
    if List.exists (fn y => x == y) [halfMinNormalPos, minPos,
                                     ~halfMinNormalPos, ~minPos]
       then ()
    else
       let
          val {exp, man} = toManExp x
          val _ =
             if true
                then
                   print (concat [exact x, " = ", exact man, " * 2^", i2s exp,
                                  "\n"])
             else ()
          val x' = fromManExp {exp = exp, man = man}
          val _ =
             if true
                then
                   print (concat ["\t = ", exact x', "\n"])
             else ()
       in
          if x == x'
             then ()
          else raise Fail "bug"
       end)

val _ = print "\nTesting split\n"

val _ =
   for (fn r =>
        let
           val {whole, frac} = split r
           val _ =
              if false
                 then
                    print (concat ["split ", exact r, " = {whole = ",
                                   exact whole, ", frac = ", exact frac, "}\n",
                                   "realMod ", exact whole, " = ",
                                   exact (realMod whole), "\t",
                                   b2s (sameSign (r, whole)), "\t",
                                   b2s (sameSign (r, frac)), "\n"])
              else ()
        in
           if realMod r == frac
              andalso realMod whole == zero
              andalso abs frac < one
              andalso sameSign (r, whole)
              andalso sameSign (r, frac)
              andalso (case class r of
                          INF => whole == r andalso frac == zero
                        | NAN => isNan whole andalso isNan frac
                        | _ => r == whole + frac)
              then ()
           else raise Fail "bug"
        end)

val _ = print "\nTesting {from,to}Large\n"

val _ =
   for
   (fn r =>
    if r == fromLarge TO_NEAREST (toLarge r)
       then ()
    else raise Fail "{from,to}Large bug")

end

structure Z = Test (structure Real = Real32
                    val size = 32)
structure Z = Test (structure Real = Real64
                    val size = 64)
