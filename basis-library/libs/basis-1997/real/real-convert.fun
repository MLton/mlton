functor RealConvert
        (structure Real: REAL) :
        REAL_1997 =
  struct
     open Real

     val class = IEEEReal1997.>> o class
     val toDecimal = IEEEReal1997.>>> o toDecimal
     val fromDecimal = fromDecimal o IEEEReal1997.<<<
  end