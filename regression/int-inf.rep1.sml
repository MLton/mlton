structure I = MLton.IntInf
val bigthree = Vector.fromList (List.map I.BigWord.fromInt [0,3])
val three = case I.fromRep (I.Big bigthree) of SOME b3 => b3 | NONE => 3
val () = if three = three + 1 - 1
         then print "That's good\n"
         else print "That's bad\n"
