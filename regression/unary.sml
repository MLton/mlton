datatype num = Z | S of num

val rec plus =
   fn (n, Z) => n
    | (n, S m) => S (plus (n,m))

val zero = Z
val one = S Z
val two = plus (one,one)

val rec times =
   fn (_, Z) => Z
    | (n, S m) => plus (n, times (n,m))

val square = fn n => times (n,n)

val four = square two

val sixteen = square four

val two56 = square sixteen

val rec fib =
   fn Z => Z
    | S Z => S Z
    | S (S n) => plus (fib (S n), fib n)

val x = fib (S Z)

