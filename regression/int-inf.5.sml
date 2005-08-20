open IntInf MLton.IntInf

fun p (a, b) = (print (toString (gcd (a, b)))
                ; print "\n")
   
val _ = List.app p [(1000, 205),
                    (1000000000000, 205),
                    (100000000000000000000, 500000000),
                    (100000000000000000000, 500000001)]
