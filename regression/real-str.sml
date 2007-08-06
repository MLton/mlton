  structure I = IEEEReal
  structure R = Real
  structure V = Word8Vector
  structure P = PackRealBig

  fun down() = I.setRoundingMode I.TO_NEGINF
  fun up() = I.setRoundingMode I.TO_POSINF

  fun word8vectorToString v = V.foldr (fn(w,s) => Word8.toString w ^ s) "" v

  val _ = down()
  val x = ~1.0/10.0
  val _ = up()
  val y = ~1.0/10.0
  val _ = if R.<=(x,y) then print "YES\n" else print "NO\n"
  val _ = print(word8vectorToString (P.toBytes x) ^ "\n")
  val _ = print(word8vectorToString (P.toBytes y) ^ "\n")

  val s = "~0.1"
  val _ = down()
  val x = Option.valOf (Real.fromString s)
  val _ = up()
  val y = Option.valOf (Real.fromString s)
  val _ = if R.<=(x,y) then print "YES\n" else print "NO\n"
  val _ = print(word8vectorToString (P.toBytes x) ^ "\n")
  val _ = print(word8vectorToString (P.toBytes y) ^ "\n")

  val da = {class = IEEEReal.NORMAL,
            sign = false,
            digits = [1],
            exp = 0}
  val _ = down()
  val x = Option.valOf (Real.fromDecimal da)
  val _ = up()
  val y = Option.valOf (Real.fromDecimal da)
  val _ = if R.<=(x,y) then print "YES\n" else print "NO\n"
  val _ = print(word8vectorToString (P.toBytes x) ^ "\n")
  val _ = print(word8vectorToString (P.toBytes y) ^ "\n")
