datatype t = A | B
datatype u = C of t

val _ = if C A = C B then raise Fail "bug" else ()
