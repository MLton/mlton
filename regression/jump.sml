
fun doit 2 = "zero"
  | doit 6 = "one" 
  | doit 10 = "two"
  | doit 14 = "three"
  | doit 18 = "four"
  | doit 22 = "five"
  | doit 26 = "six"
  | doit 30 = "seven"
  | doit 34 = "eight"
  | doit 38 = "nine"
  | doit 42 = "ten"
  | doit _ = "big"

val l = List.tabulate(50,fn i => i)

val _ = List.app (fn i => print ((doit i) ^ "\n")) l
