val _ =
   List.app (fn r =>
	     let
		val da = valOf (IEEEReal.fromString r)
		val s1 = IEEEReal.toString da
		val x = valOf (Real.fromDecimal da)
		val s2 = Real.fmt StringCvt.EXACT x
		val da' = Real.toDecimal x
		val b = Bool.toString (da = da')
	     in
		print (concat [s1, " ", s2, " ", b, "\n"])
	     end)
   ["inf", "+inF", "~iNf", "-Inf",
    "infinity", "+infinity", "~infinity", "-infinity",
    "nan", "+naN", "~nAn", "-Nan",
    "0", "0.0", "0.0E0", "~0",
    "15",
    "1.5",
    "~1.5e+1",
    "15.0",
    ".15e~2",
    ".15e-2",
    "000.0015e0",
    "1.2E999",
    "~1.2E999",
    "1E~999",
    "~1E~999",
    "1E12345678901234567890"]

