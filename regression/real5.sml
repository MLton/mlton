fun p (s: string): unit = (print s; print "\n")
fun pi (i: int): unit = p (Int.toString i)
fun pr (r: real, fmt): unit = p (Real.fmt fmt r)

open Real
datatype z = datatype StringCvt.realfmt
val _ = pi radix
val _ = pi precision

(* Avoid printing very much of maxFinite to avoid seeing differences between
 * libraries.  Someday MLton will do its own real -> string conversion and this
 * should be fixed.
 *)
val _ = p (String.substring (Real.fmt (FIX (SOME 0)) maxFinite, 0, 10))

val _ = pr (minPos, SCI (SOME 20))
val _ = pr (minNormalPos, SCI (SOME 20))
val _ = pr (posInf, SCI NONE)
val _ = pr (negInf, SCI NONE)
