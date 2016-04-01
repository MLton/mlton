val v = #[fn x => x + 1, fn x => x * 2, fn x => x]
val _ = print(Int.toString(Vector.sub (v, 1) 2))
val _ = print "\n"
