datatype num = Z | S of num

val rec plus =
   fn (n, Z) => n
    | (n, S m) => S (plus (n,m))

val rec times =
   fn (_, Z) => Z
    | (n, S m) => plus (n, times (n,m))

val rec fact =
   fn Z => S Z
    | n as S m => times (n, fact m)

val x = fact (S (S (S Z)))
              
