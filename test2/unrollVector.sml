val nums = [22, 34, 86, 17]
val nums' = Vector.fromList nums

fun demo count =
	let
		val () = if count = 0 then () else (print (Int.toString count) ; demo 0)

		val sum = Vector.foldl op +  0 nums'
	in
		print (Int.toString (sum))
	end

val () = demo 13
