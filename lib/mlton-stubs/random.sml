structure Random: MLTON_RANDOM = 
   struct
      fun seed _ = SOME (0w13: Word32.word)
      fun useed _ = SOME (0w13: Word32.word)
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
      end
   
      structure String =
	 struct
	    open String
			
	    val tabulate = CharVector.tabulate
	 end

      local
	 val chars =
	    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
	 val refresh =
	    Int.quot (Word.wordSize,
		      1 + IntInf.log2 (IntInf.fromInt (String.size chars)))
	 val r: word ref = ref (rand ())
	 val count: int ref = ref 0
	 val numChars = Word.fromInt (String.size chars)
      in
	 fun alphaNumChar (): char =
	    let
	       val n = !count
	       val _ = if 0 = n then r := rand () else ()
	       val w = !r
	       val c = String.sub (chars, Word.toInt (Word.mod (w, numChars)))
	       val _ = r := Word.div (w, numChars)
	       val n = n + 1
	       val _ = count := (if n = refresh then 0 else n)
	    in
	       c
	    end
      end

      fun alphaNumString (length: int): string =
	 String.tabulate (length, fn _ => alphaNumChar ())
   end
