datatype t = A of t

fun f (A y) = f y

fun g () = A (g ())

val _ = f (g ())
