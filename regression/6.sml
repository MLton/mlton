fun f x = x

val r: (int -> int) ref = ref f

val _ = r := (fn y => y)

val _ = !r 13 + 1
