val v = Vector.tabulate (1000, fn i => ())
val r = ref 0
val _ = r := Vector.length v
val _ = print (concat [Int.toString (!r), "\n"])
