fun tak1 (x, y, z) =
   if y >= x
      then z
   else
      tak1 (tak2 (x - 1, y, z),
	    tak2 (y - 1, z, x),
	    tak2 (z - 1, x, y))
and tak2 (x, y, z) =
   if y >= x
      then z
   else
      tak1 (tak2 (x - 1, y, z),
	    tak2 (y - 1, z, x),
	    tak2 (z - 1, x, y))

val rec f =
   fn 0 => ()
    | n => (tak1 (18, 12, 6) ; f (n-1))

val _ = f 5000
