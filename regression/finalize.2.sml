structure F = MLton.Finalize

fun loop (n, r) =
   if n = 0
      then r
   else
      let
	 val r' = ref n
	 val _ = F.finalize (r', fn () =>
			     print (concat [Int.toString (!r), "\n"]))
      in
	 loop (n - 1, r')
      end

val r = loop (10, ref 13)

