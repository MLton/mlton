structure Real =
   struct
      open Real

      local
	 open OpenInt32
      in
	 val floor = fromInt o floor
	 val ceil = fromInt o ceil
	 val trunc = fromInt o trunc
	 val round = fromInt o round
	 val radix = fromInt radix
	 val precision = fromInt precision
	 val sign = fromInt o sign
	 fun toManExp x =
	    let val {man, exp} = Real.toManExp x
	    in {man = man, exp = fromInt exp}
	    end
	 fun fromManExp{man, exp} = Real.fromManExp{man = man, exp = toInt exp}
	 fun toInt m x = fromInt(Real.toInt m x)
	 val fromInt = Real.fromLargeInt
      end
   end
