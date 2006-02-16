fun loop (left: Int.int): unit =
        case Int.compare (left, 0) of
        LESS => ()
        | EQUAL => ()
        | GREATER => loop (left + ~1)

val _ = loop 100000000

val _ = print "All ok\n"
