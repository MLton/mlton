open PackRealLittle
   
val _ =
   if List.all (fn r => Real.==(r, fromBytes(toBytes r)))
      [~100.0, ~1.1, ~0.12345, 0.0, 1.0, 123E6]
      then ()
   else raise Fail "failure"

