val e = _export "f": int * real -> char;

val _ = e (fn (i, r) =>
	   (print (concat ["i = ", Int.toString i,
			   "  r = ", Real.toString r, "\n"])
	    ; #"g"))


val g = _ffi "g": unit -> unit;
val _ = g ()
val _ = g ()
   
val e = _export "f2": Word8.word -> word array;

val _ = e (fn w => Array.tabulate (10, fn _ => Word8.toLargeWord w))

val g2 = _ffi "g2": unit -> word array;

val a = g2 ()

val _ = print (concat ["0wx", Word.toString (Array.sub (a, 0)), "\n"])

val _ = e (fn () => print "hello\n");

val g3 = _ffi "g3": unit -> unit;

val _ = g3 ()

val _ = print "success\n"
