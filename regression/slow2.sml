fun loop (left: IntInf.int): unit =
        if left = 0
           then ()
           else loop (left + ~1)

val _ = loop 100000000

val _ = print "All ok\n"
