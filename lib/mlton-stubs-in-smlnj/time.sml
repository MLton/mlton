structure Time =
   struct
      open Time

      val toSeconds = IntInf.fromInt o toSeconds
      val toMilliseconds = IntInf.fromInt o toMilliseconds
      val toMicroseconds = IntInf.fromInt o toMicroseconds
      val fromSeconds = fromSeconds o IntInf.toInt
      val fromMilliseconds = fromMilliseconds o IntInf.toInt
      val fromMicroseconds = fromMicroseconds o IntInf.toInt
      val fmt = fmt o Int32.toInt
   end
