structure Real32 =
  Real
  (structure P = Primitive.Real32
   open P
   fun fromLarge m r =
      IEEEReal.withRoundingMode (m, fn () => P.fromLarge r)
  )
