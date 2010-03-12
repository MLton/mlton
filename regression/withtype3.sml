(* withtype3.sml *)

(* Checks scoping rules of withtype *)

type u = real

datatype t = T of u * v
withtype u = int
and v = u

val z = T (1, 1.0)

val x : v = 1.0
val y : u = 1

fun uEq (a: u, b: u) = a = b
