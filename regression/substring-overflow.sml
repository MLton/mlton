open Substring

val _ =
   (slice (all "abc", 1, SOME (valOf Int.maxInt))
    ; print "ERROR\n")
   handle Subscript => print "OK\n"
