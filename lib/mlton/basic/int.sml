structure Int:
   sig
      include INTEGER

      type int
	 
      val maxInt: t
      val minInt: t
      val toReal: t -> real
   end =
   struct
      structure Int = Pervasive.Int
      structure I = Integer(open Int
			    fun divMod(a, b) = (a div b, a mod b)
			    fun quotRem(a, b) = (quot(a, b), rem(a, b))
			    val toIntInf = Pervasive.IntInf.fromInt)
      open I

      type int = t
      val maxInt = valOf Int.maxInt
      val minInt = valOf Int.minInt
      val toReal = Pervasive.Real.fromInt
   end
