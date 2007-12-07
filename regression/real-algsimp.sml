val one = valOf (Real.fromString "1.0")
val zero = valOf (Real.fromString "0.0")
val posInf = one / zero
val negInf = ~one / zero

val nan1 = posInf + negInf

fun cmp f = print ((Bool.toString (f (nan1, nan1))) ^ "\n")

local
   open Real
in
   val _ = cmp (op <)
   val _ = cmp (op <=)
   val _ = cmp (op >)
   val _ = cmp (op >=)
   val _ = cmp (op ==)
   val _ = cmp (op !=)
   val _ = cmp (op ?=)
end


val nan2 = valOf (Real.fromString "nan")

fun cmp f = print ((Bool.toString (f (nan1, nan2))) ^ "\n")

local
   open Real
in
   val _ = cmp (op <)
   val _ = cmp (op <=)
   val _ = cmp (op >)
   val _ = cmp (op >=)
   val _ = cmp (op ==)
   val _ = cmp (op !=)
   val _ = cmp (op ?=)
end
