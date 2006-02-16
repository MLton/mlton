open Vector

fun assert(msg,b) =
   (if b then ()
    else print("assertion failed: " ^ msg ^ "\n"))

val v1 = fromList[1,2,3]
val v2 = fromList[1,2,3]

val _ = assert("vector equality",
               v1 = v2
               andalso fromList[v1, v2] = fromList[v2, v1]
               andalso v1 <> fromList[1,2]
               andalso v1 <> fromList[1,2,4])

open Array

val a1 = fromList[1,2,3]
val a2 = fromList[1,2,3]

val _ = assert("array equality", a1 <> a2)
