fun loop (left: IntInf.int): unit =
        case IntInf.compare (left, 4294967296) of
        LESS => ()
        | EQUAL => ()
        | GREATER => loop (left + ~1)

val _ = loop 4304967296

val _ = print "All ok\n"
