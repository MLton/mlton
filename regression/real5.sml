fun p (s: string): unit = (print s; print "\n")
fun pi (i: int): unit = p (Int.toString i)
fun pr (r: real, fmt): unit = p (Real.fmt fmt r)

open Real
datatype z = datatype StringCvt.realfmt
val _ = pi radix
val _ = pi precision
val _ = pr (maxFinite, FIX (SOME 0))
val _ = pr (minPos, SCI (SOME 20))
val _ = pr (minNormalPos, SCI (SOME 20))
val _ = pr (posInf, SCI NONE)
val _ = pr (negInf, SCI NONE)
