val x = "abc"
val y = "defg"
val _ =
   if 3 = (String.size
           (if 0 = length (CommandLine.arguments ())
               then x
            else y))
      then ()
   else raise Fail "bug"
