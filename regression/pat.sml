val c: int as d: int = 13
val c: unit -> unit as d: unit -> unit = fn () => ()
val c: 'a -> unit as d: 'a -> unit = fn _ => ()

val (f, hd::tail) = (fn x => x, [fn y => (y,y)])

val (s,_) = f (hd "hello world\n")

val _ = print (s)


