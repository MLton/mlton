structure Error: ERROR =
struct
   
fun unimplemented msg = raise Fail(concat["unimplemented: ", msg])

fun bug msg = raise(Fail msg)

fun warning msg = TextIO.output(TextIO.stdErr, msg^"\n")

end
