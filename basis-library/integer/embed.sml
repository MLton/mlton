functor EmbedInt (structure Big: INTEGER
		  structure Small:
		     sig
			eqtype int

			val precision': Int.int
			val fromBigUnsafe: Big.int -> int
			val toBig: int -> Big.int
		     end): INTEGER =
   struct
      open Small
	 
      val precision = SOME precision'

      val maxIntBig =
	 Big.fromLarge
	 (LargeInt.- (Word.toLargeInt (Word.<<
				       (0w1,
					Word.fromInt (Int.- (precision', 1)))),
		      1))

      val maxInt = SOME (fromBigUnsafe maxIntBig)

      val minIntBig = Big.- (Big.~ maxIntBig, Big.fromInt 1)

      val minInt = SOME (fromBigUnsafe minIntBig)

      fun fromBig (i: Big.int): int = 
	 if Big.<= (minIntBig, i) andalso Big.<= (i, maxIntBig)
	    then fromBigUnsafe i
	 else raise Overflow

      local
	 val make: (Big.int * Big.int -> Big.int) -> (int * int -> int) =
	    fn f => fn (x, y) => fromBig (f (toBig x, toBig y))
      in
	 val op * = make Big.*
	 val op + = make Big.+
	 val op - = make Big.-
	 val op div = make Big.div
	 val op mod = make Big.mod
	 val quot = make Big.quot
	 val rem = make Big.rem
      end

      local
	 val make: (Big.int * Big.int -> 'a) -> (int * int -> 'a) =
	    fn f => fn (x, y) => f (toBig x, toBig y)
      in
	 val op < = make Big.<
	 val op <= = make Big.<=
	 val op > = make Big.>
	 val op >= = make Big.>=
	 val compare = make Big.compare
      end

      val fromInt = fromBig o Big.fromInt

      val toInt = Big.toInt o toBig

      local
	 val make: (Big.int -> Big.int) -> (int -> int) =
	    fn f => fn x => fromBig (f (toBig x))
      in
	 val ~ = make Big.~
	 val abs = make Big.abs
      end

      fun fmt r i = Big.fmt r (toBig i)

      val fromLarge = fromBig o Big.fromLarge

      fun fromString s = Option.map fromBig (Big.fromString s)

      fun max (i, j) = if i >= j then i else j

      fun min (i, j) = if i <= j then i else j

      fun scan r reader state =
	 Option.map
	 (fn (i, state) => (fromBig i, state))
	 (Big.scan r reader state)
	 
      val sign = Big.sign o toBig

      fun sameSign (x, y) = sign x = sign y

      val toLarge = Big.toLarge o toBig

      val toString = Big.toString o toBig
   end

structure Int31 = EmbedInt (structure Big = Int32
			    structure Small = Primitive.Int31)
