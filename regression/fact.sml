val rec fact =
   fn 0 => 1
    | n => n * fact(n - 1)

val _ = print(concat[Int.toString(fact 10), "\n"])
