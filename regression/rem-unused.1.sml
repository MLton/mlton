datatype ttt = AAA | BBB of ttt vector

val z = AAA
val y = BBB (Vector.tabulate (1, fn _ => z))
(* val x = BBB (Vector.tabulate (1, fn _ => z)) *)
(* val _ = print (concat [Bool.toString (y = x), "\n"]) *)

val a : ttt vector = Vector.tabulate (0, fn _ => raise Fail "a")
val b : ttt vector = Vector.tabulate (0, fn _ => raise Fail "b")

val _ = print (concat [Bool.toString (a = b), "\n"])
