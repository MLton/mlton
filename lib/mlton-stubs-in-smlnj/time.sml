structure Time =
   struct
      open Time

      val fmt = fmt o Int32.toInt
   end
