structure Real =
   struct
      open Real
	 
      val fromLargeInt: IntInf.int -> real =
	 fn _ => raise Fail "Real.fromLargeInt"
      val toLargeInt: IEEEReal.rounding_mode -> real -> IntInf.int =
	 fn _ => fn _ => raise Fail "Real.toLargeInt"
   end
