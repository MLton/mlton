(* overloading.sml *)

(* Checks overloading resolution. *)

val z = 1: Int16.int
val y = z + 2
val x = (valOf Int16.minInt) + z

fun f(x,y) = (x + y)/y
fun g(x,y) = x/(x - y)

val x = f(1.2, 2.3) + g(1.0, 2.0);

fun f x =
x + let
        fun g() = x
    in
        g() * 2.0
    end;
