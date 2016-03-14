val text = TextIO.inputLine TextIO.stdIn
val gotText = case text of NONE => false | _ => true
fun loop n i s =
	if i <  n then
		let
		val a = if gotText then 1337
			else 9001
		in
			loop n (i+1) (s+a)
		end
	else
		s

val () = print (Int.toString (loop 13 0 0))
