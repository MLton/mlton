open Vector

datatype t = T of t vector
fun makeT() = T(tabulate(0, fn _ => makeT()))
fun destT(T v) =
   if length v > 0
      then 1 + destT(sub(v, 0))
   else 0
val _ =
   if 0 = destT(makeT())
      then ()
   else raise Fail "bug"
