(* withtype.sml *)

(* Checks scoping rules of withtype *)

type u = int

datatype t = T of u * v
withtype u = bool
and      v = u

val z = T(true, 6)
val y : u = true
val x : v = 1

fun tEq (a: t, b: t) = a = b
fun uEq (a: u, b: u) = a = b
fun vEq (a: v, b: v) = a = b
