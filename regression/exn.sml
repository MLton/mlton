exception E

fun loop n =
   if n = 0
      then raise E
   else (loop(n - 1) handle e => (print "z"; raise e))

val _ = loop 13 handle _ => print "\n"
