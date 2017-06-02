local
fun f op= () = ()
fun op= () = ()
val rec f = fn op= => fn () => ()
val rec op= = fn () => ()
in end
