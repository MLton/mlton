fun f n =
   if 0 = Array.sub (Array.tabulate (n, fn i => i), 0)
      then ()
   else f 13

val _ = (f 0; raise Fail "bug") handle Subscript => ()
val _ = f 1
