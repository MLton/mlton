(* Checks overload declarations. *)

fun f (x: int) = x
fun g (x: word) = x

_overload f: 'a -> 'a as f and g

val _ = f 1
val _ = f 0w1
