structure Random: MLTON_RANDOM = 
   struct
      fun seed _ = 0w13: Word32.word
      fun useed _ = 0w13: Word32.word
      local
	 val seed: word ref = ref 0w13
      in
	 (* From page 284 of Numerical Recipes in C. *)
	 fun rand (): word =
	    let
	       val res = 0w1664525 * !seed + 0w1013904223
	       val _ = seed := res
	    in
	       res
	    end

	 fun srand (w: word): unit = seed := w

	 structure String =
	    struct
	       open String
			
	       val tabulate = CharVector.tabulate
	    end
	 local
	    val chars =
	       "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
	    val n = Word.fromInt (String.size chars)
	    val r: word ref = ref 0w0
	 in
	    fun alphaNumString (length: int) =
	       String.tabulate
	       (length, fn i =>
		let
		   val _ =
		      if 0 = Int.quot (i, 6) (* n^6 = 62^6 = 965,660,736 *)
			 then r := rand ()
		      else ()
		   val w = !r
		   val c = String.sub (chars, Word.toInt (Word.mod (w, n)))
		   val _ = r := Word.div (w, n)
		in
		   c
		end)
	 end
      end
   end
