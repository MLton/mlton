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

      local
	fun pow2' (n, w) =
	  if w = 0wx0
	    then n
	  else pow2' (if Pervasive.Word32.andb (0wx1, w) = 0wx1
			then (Pervasive.IntInf.fromInt 2) * n else n,
		      Pervasive.Word32.>> (w, 0wx1))
	fun pow2 w = pow2' (Pervasive.IntInf.fromInt 1, w)
      in
	val (op <<) = fn (a, b) => a * (pow2 b)
	val (op ~>>) = fn (a, b) => a div (pow2 b)
      end
   end

structure LargeInt = IntInf
   
structure Int =
   struct
      open PreInt
      val toLarge = IntInf.fromInt
      val fromLarge = IntInf.toInt
   end

structure Int32 = Int

