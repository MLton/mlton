fun r2s x = Real.fmt (StringCvt.SCI (SOME 20)) x
val i2s = Int.toString
	 
fun doit x =
   let
      val {exp, man} = Real.toManExp x
      val _ = print (concat [r2s x, " = ", r2s man, " * 2^", i2s exp, "\n"])
      val _ =
	 print (concat ["\t = ",
			r2s (Real.fromManExp {exp = exp, man = man}),
			"\n"])
   in
      ()
   end

val _ =
   app doit [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0, 10.0, Real.maxFinite]
