fun r2s x = Real.fmt (StringCvt.FIX (SOME 20)) x

val x = 1.0
val y = 2.0
val z = Real.nextAfter (x, y)
val _ = print (concat [r2s z, " = nextAfter (", r2s x, ", ", r2s y, ")\n"])

