fun doit (x, mode, name) =
   let
      val i = Real.toLargeInt mode x
   in
      print (concat [name, "\t",
		     Real.fmt (StringCvt.FIX (SOME 2)) x, "\t",
		     IntInf.toString i, "\n"])
   end

val _ =
   app
   (fn (mode, name) =>
    app (fn x =>
	 (doit (x, mode, name)
	  ; doit (~ x, mode, name)
	  ; doit (1E12 + x, mode, name)
	  ; doit (~1E12 + x, mode, name)))
    [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0])
   (let
       datatype z = datatype IEEEReal.rounding_mode
    in
       [(TO_NEAREST, "nearest"),
	(TO_NEGINF, "neginf"),
	(TO_POSINF, "posinf"),
	(TO_ZERO, "zero")]
    end)
