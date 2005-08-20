(* This program tests the implementation of arrays whose length is important but
 * whose elements aren't.
 *)
open Array
val a = array (2, #"a")
val _ = update (a, 0, #"b")
val n = if sub (a, 0) = #"b"
           then 2
        else 1
val _ =
   if 2 = length (array (n, 13))
      then ()
   else raise Fail "bug"
