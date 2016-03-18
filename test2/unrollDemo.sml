fun demo count =
	let
		val () = if count = 0 then () else (print (Int.toString count) ; demo 0)

		fun loop n i s =
			if i <  n then
				loop n (i+1) (s+90)
			else
				s
	in
		print (Int.toString (loop 5 0 0))
	end

val () = demo 13
