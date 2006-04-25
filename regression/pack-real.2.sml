
val v =
   Word8Vector.fromList
   [0wx0D,0wxE5,0wx35,0wx94,0wxD7,0wx00,0wx82,0wx40,
    0wx0D,0wxE5,0wx35,0wx94,0wxD7,0wx00,0wx82,0wx40]

val r = PackReal64Little.subVec(v, 0)
val () = print (concat [Real64.toString r, "\n"])
val r = PackReal64Little.subVec(v, 1)
val () = print (concat [Real64.toString r, "\n"])

val r = PackReal64Big.subVec(v, 0)
val () = print (concat [Real64.toString r, "\n"])
val r = PackReal64Big.subVec(v, 1)
val () = print (concat [Real64.toString r, "\n"])
