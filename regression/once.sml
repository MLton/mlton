open SMLofNJ.Cont

val r: unit cont option ref = ref NONE

val a = ref 13
   
val () = callcc(fn k => r := SOME k)

val b = ref 13

val _ = a := !a - 1
val _ = b := !b - 1

val _ = print(concat["a = ", Int.toString(!a),
                     "  b = ", Int.toString(!b),
                     "\n"])

val _ = if !a = 0
           then ()
        else throw (valOf(!r)) ()
