datatype t = A | B

val f =
   fn A => 1
    | B => 2

val _ = f(raise Overflow)
val _ = f(raise Bind)
