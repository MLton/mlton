datatype t = Z | S of t;
val zero = Z;
val one = S zero;
val two = S one;
val rec add: t * t -> t = fn (Z, n) => n | (S m, n) => S (add (m, n));
val rec mul: t * t -> t = fn (Z, n) => Z | (S z, n) => add (n, mul (z, n));
val four = mul (two, two);
val rec exp: t * t -> t = fn (n, Z) => one | (n, S m) => mul (n, exp (n, m));
val _ = exp (exp (four, four), two);
