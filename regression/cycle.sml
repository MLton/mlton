datatype t = T of u | V
and u = U of t * t

fun f V = T (U (f V,f V))
  | f (T _) = V

val _ = f V
