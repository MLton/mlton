val _ =
   if Word32.- (0w0, 0w1) = 0wxFFFFFFFF
      then ()
   else raise Fail "bug"

fun loop i =
   if i = 100
      then ()
   else if let val w = Word32.fromInt i
	   in 0w0 - w = 0wxFFFFFFFF - w + 0w1
	   end
	   then loop (i + 1)
	else raise Fail "bug"

val _ = loop 0

val _ =
   if Word8.- (0w0, 0w1) = 0wxFF
      then ()
   else raise Fail "bug"

fun loop i =
   if i = 256
      then ()
   else if 0w0 - Word8.fromInt i = Word8.fromInt (Int.- (256, i))
	   then loop (i + 1)
	else raise Fail "bug"

val _ = loop 0
