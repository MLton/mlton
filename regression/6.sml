fun f x = x

val r = ref f

val _ = r := (fn y => y)

val _ = !r 13 + 1
