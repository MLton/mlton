datatype t = T of t

datatype u = A of int | B of t * int

fun f () = T (f ())

val _ =
   case if 0 = 0 then B (f (), 13) else A 13 of
      B (_, n) => n + 1
