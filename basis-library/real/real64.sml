structure Real64 =
  Real
  (structure P = Primitive.Real64
   open P
   fun fromLarge m r = P.fromLarge r
   val negInf = ~1.0 / 0.0
   val posInf = 1.0 / 0.0
   fun nextAfterDown r = nextAfter (r, negInf)
   fun nextAfterUp r = nextAfter (r, posInf)
  )
structure Real = Real64
val real = Real.fromInt
structure RealGlobal: REAL_GLOBAL = Real
open RealGlobal
structure LargeReal = Real64
