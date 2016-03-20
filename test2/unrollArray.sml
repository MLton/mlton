val nums = [22, 34, 86, 17]
val nums' = Array.fromList nums

fun demo count =
	let
		val () = if count = 0 then () else (print (Int.toString count) ; demo 0)

		val sum = Array.foldl op +  0 nums'
	in
		print (Int.toString (sum))
	end

val () = demo 13
