functor Ring(S: RING_STRUCTS):> RING where type t = S.t = 
struct

open S

fun a - b = a + (~ b)

fun isZero a = equals(a, zero)

fun double n = n + n

fun square n = n * n

fun sum l = List.fold(l, zero, op +)
   
end
