
val _ = MLton.Profile.write "mlmon.init.out"
val _ = MLton.Profile.reset ()

val rec fib =
   fn 0 => 0
    | 1 => 1
    | n => fib (n - 1) + fib (n - 2)

val rec f =
   fn 0 => ()
    | n => (fib 38; f (n-1))

val _ = f 2
val _ = MLton.Profile.write "mlmon.fib.out"
val _ = MLton.Profile.reset ()

fun tak (x,y,z) =
   if not (y < x)
      then z
   else tak (tak (x - 1, y, z),
             tak (y - 1, z, x),
             tak (z - 1, x, y))

val rec g =
   fn 0 => ()
    | n => (tak (18,12,6); g (n-1))

val _ = g 500
val _ = MLton.Profile.write "mlmon.tak.out"
val _ = MLton.Profile.reset ()
