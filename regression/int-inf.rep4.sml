structure I = MLton.IntInf

val big_canonical = Vector.fromList (List.map I.BigWord.fromInt [0,0,1])
val big_canonical = valOf (I.fromRep (I.Big big_canonical))

val big_non_canon = Vector.fromList (List.map I.BigWord.fromInt [0,0,1,0])
val big_non_canon = case I.fromRep (I.Big big_non_canon) of SOME bnc => bnc | NONE => big_canonical

val () = case IntInf.compare (big_canonical, big_non_canon) of
         EQUAL => print "That's good\n"
         | _ => print "That's bad\n"

val () = print (IntInf.toString big_canonical^"\n")
val () = print (IntInf.toString big_non_canon^"\n")
