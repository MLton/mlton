functor IEEERealConvert
        (structure IEEEReal: IEEE_REAL):
        sig
	  include IEEE_REAL_1997 
	  val >> : IEEEReal.float_class -> float_class
	  val << : float_class -> IEEEReal.float_class
	end =
  struct
     open IEEEReal

     datatype nan_mode = QUIET | SIGNALLING
     datatype float_class =
        NAN of nan_mode
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL
     val >> =
       fn IEEEReal.NAN => NAN QUIET
	| IEEEReal.INF => INF
	| IEEEReal.ZERO => ZERO
	| IEEEReal.NORMAL => NORMAL
	| IEEEReal.SUBNORMAL => SUBNORMAL
     val << =
       fn NAN _ => IEEEReal.NAN
	| INF => IEEEReal.INF
	| ZERO => IEEEReal.ZERO
	| NORMAL => IEEEReal.NORMAL
	| SUBNORMAL => IEEEReal.SUBNORMAL

     type decimal_approx = {kind: float_class, sign: bool,
			    digits: int list, exp: int}
     val toString = fn {kind, sign, digits, exp} =>
       toString {class = << kind, sign = sign, 
		 digits = digits, exp = exp}
     val fromString = fn s =>
       Option.map
       (fn {class, sign, digits, exp} =>
	{kind = >> class, sign = sign, 
	 digits = digits, exp = exp})
       (fromString s)
  end
