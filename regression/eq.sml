fun f x = x + 1
fun g x = x + 2

val _  =
   if MLton.eq (f, g)
      then print "yes\n"
   else print "no\n"
