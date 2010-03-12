(* withtype2.sml *)

(* Checks scoping rules of withtype *)

type u = real

datatype t = A of v | B of u
withtype u = int
and v = u

val a = A 1.0
val b = B 1

val x : v = 1.0
val y : u = 1

fun uEq (a: u, b: u) = a = b
