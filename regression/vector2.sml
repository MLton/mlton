open Vector
val v = tabulate(13, fn i => fn j => i + j)
val _ = print(Int.toString(sub(v, 5) 1))
val _ = print "\n"
