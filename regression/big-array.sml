open Array

val a'' = tabulate (1000000, fn i => i)
val _ = sub (a'', 0) + sub (a'', 1)
val _ = print "OK\n"
