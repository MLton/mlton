fun tak (x, y, z) =
   if y >= x
      then z
   else
      ((12345678: IntInf.int) * 123456789
       ; tak (tak (x - 1, y, z),
	      tak (y - 1, z, x),
	      tak (z - 1, x, y)))

val rec f =
   fn 0 => ()
    | n => (tak (18, 12, 6) ; f (n-1))

val _ = f 5000
