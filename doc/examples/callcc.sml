open MLton.Cont
   
val _ = print(Int.toString(1 + callcc(fn k => throw(k, 2))))
val _ = print "\n"
