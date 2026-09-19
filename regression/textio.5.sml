fun optionToString toString opt =
   case opt of
      NONE => "NONE"
    | SOME v => concat ["SOME ", toString v]

val intOptionToString = optionToString Int.toString

val () = TextIO.print ("getInstream (openString \"abcdef\") = s0\n")
val s0 = TextIO.getInstream (TextIO.openString "abcdef")
val (v, s1) = TextIO.StreamIO.input s0
val () = TextIO.print ("input s0 = (\"" ^ String.toString v ^ "\", s1)\n")
val () = TextIO.print ("canInput (s0, 1) = " ^ intOptionToString (TextIO.StreamIO.canInput (s0, 1)) ^ "\n")
val () = TextIO.print ("canInput (s1, 1) = " ^ intOptionToString (TextIO.StreamIO.canInput (s1, 1)) ^ "\n")

val () = TextIO.print "\n"

val () = TextIO.print ("getInstream (openString \"abcdef\") = s0\n")
val s0 = TextIO.getInstream (TextIO.openString "abcdef")
val () = TextIO.print ("canInput (s0, 1) = " ^ intOptionToString (TextIO.StreamIO.canInput (s0, 1)) ^ "\n")
val (v, s1) = TextIO.StreamIO.input s0
val () = TextIO.print ("input s0 = (\"" ^ String.toString v ^ "\", s1)\n")
val () = TextIO.print ("canInput (s1, 1) = " ^ intOptionToString (TextIO.StreamIO.canInput (s1, 1)) ^ "\n")
