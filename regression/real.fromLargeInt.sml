fun loop (i, n) =
   if n = 0
      then ()
   else
      let
	 val x = Real.fromLargeInt i
      in
	 print (concat [IntInf.toString i,
			" = ",
			Real.fmt (StringCvt.SCI (SOME 10)) x,
			"\n"])
	 ; loop (i * 10, n - 1)
      end

val _ = loop (1, 310)
