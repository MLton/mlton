structure IntInf: INT_INF =
   struct
      open Pervasive.IntInf

      val toInt = toLarge
      val sign = Pervasive.Int32.fromInt o sign
      val fromInt = fromLarge
      val divMod = divmod
      val quotRem = quotrem
      val precision: Pervasive.Int32.int option = NONE
      fun toLarge x = x
      fun fromLarge x = x
      val log2 = Pervasive.Int32.fromInt o log2
      fun pow (a, b) = Pervasive.IntInf.pow (a, Pervasive.Int32.toInt b)
   end

structure LargeInt = IntInf
   
structure Int =
   struct
      open PreInt
      val toLarge = IntInf.fromInt
      val fromLarge = IntInf.toInt
   end

structure Int32 = Int

