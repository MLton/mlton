open MLton.World

val _ =
   case save "world" of
      Original => print "I am the original\n"
    | Clone => print "I am the clone\n"
