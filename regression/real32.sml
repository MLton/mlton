datatype z = datatype IEEEReal.rounding_mode

val _ =
   List.app
   (fn r =>
    List.app
    (fn m =>
     let
	val r' = Real32.toLarge (Real32.fromLarge m r)
     in
	print (concat [Real.fmt StringCvt.EXACT r,
		       " ",
		       Real.fmt StringCvt.EXACT r',
		       "\n"])
     end)
    [TO_NEAREST, TO_NEGINF, TO_POSINF, TO_ZERO])
   [Real.negInf,
    ~ Real.maxFinite,
    ~1.0,
    ~ Real.minNormalPos,
    ~ Real.minPos,
    0.0,
    Real.minPos,
    Real.minNormalPos,
    1.0,
    Real.maxFinite,
    Real.posInf,
    Real.posInf + Real.negInf]
   
