structure OpenInt32 =
   struct
      val toInt = Pervasive.Int32.toInt
      val fromInt = Pervasive.Int32.fromInt
      val toIntOpt =
	 fn NONE => NONE
	  | SOME i => SOME(toInt i)
   end
