structure MLtonRandom: MLTON_RANDOM =
   struct
      (* Linux specific.  Uses /dev/random and /dev/urandom to get a
       * random word.
       *)
      local
	 fun make (file, name) =
	    let
	       val buf = Word8Array.array (4, 0w0)
	    in
	       fn () =>
	       let
		  val fd =
		     let
			open Posix.FileSys
		     in
			openf (file, O_RDONLY, O.flags [])
		     end
		  fun loop rem =
		     let
			val n = Posix.IO.readArr (fd, {buf = buf,
						       i = 4 - rem,
						       sz = SOME rem})
			val _ = if n = 0
				   then (Posix.IO.close fd; raise Fail name)
				else ()
			val rem = rem - n
		     in
			if rem = 0
			   then ()
			else loop rem
		     end
		  val _ = loop 4
		  val _ = Posix.IO.close fd
	       in
		  Pack32Little.subArr (buf, 0)
	       end
	    end
      in
	 val seed = make ("/dev/random", "Random.seed")
	 val useed = make ("/dev/urandom", "Random.useed")
      end

      local
	 open Word
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
