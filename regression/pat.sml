
val (f, hd::tail) = (fn x => x, [fn y => (y,y)])

val (s,_) = f (hd "hello world\n")

val _ = print (s)


