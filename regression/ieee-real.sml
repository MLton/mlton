structure R = Real
structure I = IEEEReal
structure V = Word8Vector
structure P = PackRealBig

fun setRM mode =
   (I.setRoundingMode mode;
    if I.getRoundingMode () <> mode
       then raise Fail "setRM"
    else ())

fun up() = setRM I.TO_POSINF
fun down() = setRM I.TO_NEGINF
fun near() = setRM I.TO_NEAREST
fun zero() = setRM I.TO_ZERO

  (*

  61 digits of 1 / 10 in Mathematica

  In[7]:= RealDigits[1 / 10,2,61]

  Out[7]= {{1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
               0, 1, 1, 0, 0, 1, 1, 0, 0, 1,
               1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
               0, 1, 1, 0, 0, 1, 1, 0, 0, 1,
               1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
               0, 1, 1, 0, 0, 1, 1, 0, 0, 1},-3}

  *)


val mtenth_lo = (down();1.0 / 10.0)
val mtenth_hi = (up();1.0 / 10.0)
val mtenth_near = (near();1.0 / 10.0)
val mtenth_zero = (zero();1.0 / 10.0)

fun word8vectorToString v = V.foldr (fn(w,s) => Word8.toString w ^ s) "" v

val _ = print(word8vectorToString (P.toBytes mtenth_lo) ^ "\n")
val _ = print(word8vectorToString (P.toBytes mtenth_hi) ^ "\n")
val _ = print(word8vectorToString (P.toBytes mtenth_near) ^ "\n")
val _ = print(word8vectorToString (P.toBytes mtenth_zero) ^ "\n")
