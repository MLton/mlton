open Array MLton.GC

val a = tabulate (100000, fn i => i)
val _ = pack ()
val a' = tabulate (100, fn i => i)
val _ = unpack ()
val _ = sub (a, 0) + sub (a, 1)
val _ = sub (a', 0) + sub (a', 1)
val a'' = tabulate (1000000, fn i => i)
val _ = sub (a'', 0) + sub (a'', 1)
val _ = print "OK\n"
