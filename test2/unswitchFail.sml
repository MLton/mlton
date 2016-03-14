val text = TextIO.inputLine TextIO.stdIn
val gotText = case text of NONE => false | _ => true
fun loop n i s =
	if i <  n then
		let
		val a = case gotText of
			  true =>  1337
			| _ => raise Fail("oops")
		in
			loop n (i+1) (s+a)
		end
	else
		0

val () = print (Int.toString (loop 13 0 0))
