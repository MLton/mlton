fun append (l1, l2) =
   case l1 of
      [] => l2
    | x :: l1 => x :: append (l1, l2)

fun rev l =
   case l of
      [] => []
    | x :: l => append (rev l, [x])

val l = List.tabulate (1000, fn i => i)
val _ = 1 + hd (rev l)
