(* withtype.sml *)

(* Checks scoping rules of withtype *)

type u = int

datatype t = T of u * v
withtype u = bool
and      v = u

val x = T(true, 6);
