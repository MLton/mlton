fun rev l =
   case l of
      [] => []
    | x :: l => rev l @ [x]

val l = List.tabulate (100, fn i => i)
val _ = 1 + hd (rev l)
