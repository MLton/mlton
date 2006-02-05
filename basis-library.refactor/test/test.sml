
fun printString s =
   PrimitiveFFI.Stdio.print s

fun printIntInf i =
   let
      val s = Primitive.IntInf.toString8 i
   in
      printString s
   end

local
   open Primitive.IntInf
in
   fun fact n =
      if n = 0 then 1 else n * (fact (n - 1))
end

val () = (printString "fact 40 = "
          ; printIntInf (fact 40)
          ; printString "\n")
