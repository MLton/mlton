structure Real64 =
  Real
  (structure P = Primitive.Real64
   open P
   fun fromLarge m r = P.fromLarge r
  )
structure Real = Real64
val real = Real.fromInt
structure RealGlobal: REAL_GLOBAL = Real
open RealGlobal
structure LargeReal = Real64
