
fun nlist 0 = 0::nil
  | nlist n = n::(nlist (n-1))

val rec last =
   fn nil => 0
    | x::nil => x
    | _ :: l => last l

val n = 1 + (last (nlist (10)))

val _ = print ((Int.toString n) ^ "\n")
