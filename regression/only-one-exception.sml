fun f n =
   if n = 0
      then 0
   else f (f (n - 1))

val _ = f 13

fun loop () = loop ()

val _ = loop ()
