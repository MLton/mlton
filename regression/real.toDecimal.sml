val {class, digits, exp, sign} = Real.toDecimal 1234.567

datatype z = datatype IEEEReal.float_class
   
val class =
   case class of
      NAN => "NAN"
    | INF => "INF"
    | ZERO => "ZERO"
    | NORMAL => "NORMAL"
    | SUBNORMAL => "SUBNORMAL"

val digits = concat (List.map Int.toString digits)

val exp = Int.toString exp

val sign = if sign then "~" else ""

val _ = print (concat [class, "  ", sign, "0.", digits, "E", exp, "\n"])
