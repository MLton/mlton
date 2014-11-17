structure I = MLton.IntInf
val bigthree = Vector.fromList (List.map I.BigWord.fromInt [0,3])
val three = case I.fromRep (I.Big bigthree) of SOME b3 => b3 | NONE => 3
val () = case IntInf.compare (three, three + 1 - 1) of
         EQUAL => print "That's good\n"
         | _ => print "That's bad\n"
