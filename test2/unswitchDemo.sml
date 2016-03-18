fun demo (count, b) =
	let
		val () = if count = 0 then () else (print (Int.toString count) ; demo (0, b))

		fun loop n i s =
			if i <  n then
				let
					val a = if b then 1337
							else 9001
				in
					loop n (i+1) (s+a)
				end
			else
				s
	in
		print (Int.toString (loop count 0 0))
	end

val x = Option.valOf (Int.fromString (Option.valOf (TextIO.inputLine TextIO.stdIn)))
val text = TextIO.inputLine TextIO.stdIn
val gotText = case text of NONE => false | _ => true
val () = demo (x, gotText)
