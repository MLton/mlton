(* See https://github.com/MLton/mlton/issues/649. *)

val s = TextIO.openString "x"
val () = TextIO.print ("openString \"x\"\n")
val () = TextIO.print ("endOfStream = " ^ Bool.toString (TextIO.endOfStream s) ^ "\n")
val () = TextIO.print ("inputAll = \"" ^ String.toString (TextIO.inputAll s) ^ "\"\n")
val () = TextIO.print ("endOfStream = " ^ Bool.toString (TextIO.endOfStream s) ^ "\n")
val () = TextIO.print ("inputAll = \"" ^ String.toString (TextIO.inputAll s) ^ "\"\n")
val () = TextIO.print ("endOfStream = " ^ Bool.toString (TextIO.endOfStream s) ^ "\n")

val () = TextIO.print "\n"

val s = TextIO.openString "abc"
val () = TextIO.print ("openString \"abc\"\n")
val () = TextIO.print ("endOfStream = " ^ Bool.toString (TextIO.endOfStream s) ^ "\n")
val () = TextIO.print ("input1 = \"" ^ Char.toString (valOf (TextIO.input1 s)) ^ "\"\n")
val () = TextIO.print ("endOfStream = " ^ Bool.toString (TextIO.endOfStream s) ^ "\n")
val () = TextIO.print ("inputAll = \"" ^ String.toString (TextIO.inputAll s) ^ "\"\n")
val () = TextIO.print ("endOfStream = " ^ Bool.toString (TextIO.endOfStream s) ^ "\n")
val () = TextIO.print ("inputAll = \"" ^ String.toString (TextIO.inputAll s) ^ "\"\n")
val () = TextIO.print ("endOfStream = " ^ Bool.toString (TextIO.endOfStream s) ^ "\n")
