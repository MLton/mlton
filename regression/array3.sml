open Array

val a = array (1000, 0)

fun g i = (update (a, i, i); g (i + 1))
   
fun f i =
   if i = 100
      then g i
   else (update (a, i, 0); f (i + 1))

val _ = f 0 handle Subscript => ()

val _ = sub (a, 0) + 1
