structure Error: ERROR =
struct
   
fun unimplemented msg = raise Fail(concat["unimplemented: ", msg])

fun bug msg = raise(Fail msg)

end
