structure LargeWord:> WORD = Word32
   
signature WORD =
   sig
      eqtype word
	 
      val * : word * word -> word 
      val + : word * word -> word 
      val - : word * word -> word 
      val < : word * word -> bool 
      val << : word * Word32.word -> word 
      val <= : word * word -> bool 
      val > : word * word -> bool 
      val >= : word * word -> bool 
      val >> : word * Word32.word -> word 
      val ~ : word -> word
      val ~>> : word * Word32.word -> word 
      val andb: word * word -> word 
      val compare: word * word -> order 
      val div: word * word -> word 
      val fmt: StringCvt.radix -> word -> string 
      val fromInt: Int32.int -> word 
      val fromLarge: LargeWord.word -> word 
      val fromLargeInt: IntInf.int -> word
      val fromLargeWord: LargeWord.word -> word 
      val fromString: string -> word option 
      val max: word * word -> word
      val min: word * word -> word 
      val mod: word * word -> word
      val notb: word -> word 
      val orb: word * word -> word 
      val scan:
	 StringCvt.radix
	 -> (char, 'a) StringCvt.reader
	 -> (word, 'a) StringCvt.reader
      val toInt: word -> Int32.int 
      val toIntX: word -> Int32.int 
      val toLarge: word -> LargeWord.word
      val toLargeInt: word -> IntInf.int 
      val toLargeIntX: word -> IntInf.int 
      val toLargeWord: word -> LargeWord.word 
      val toLargeWordX: word -> LargeWord.word 
      val toLargeX: word -> LargeWord.word
      val toString: word -> string 
      val wordSize: Int32.int 
      val xorb: word * word -> word 
   end

functor FixWord (W: PERVASIVE_WORD): WORD =
   struct
      local
	 open W
      in
	 type word = word
	 val op * = op *
	 val op + = op +
	 val op - = op -
	 val op < = op <
	 val op <= = op <=
	 val op > = op >
	 val op >= = op >=
	 val ~ = ~
	 val andb = andb
	 val compare = compare
	 val op div = op div
	 val fromString = fromString
	 val max = max
	 val min = min
	 val op mod = op mod
	 val notb = notb
	 val orb = orb
	 val scan = scan
	 val xorb = xorb
      end
   
      val wordSize = Pervasive.Int32.fromInt W.wordSize
	 
      local
	 fun fix (f: word * Word31.word -> word)
	    (w: word, w': Word32.word): word =
	    f (w, Word31.fromLargeWord w')
      in
	 val << = fix W.<<
	 val >> = fix W.>>
	 val ~>> = fix W.~>>
      end
      val fromInt = W.fromLargeInt o Pervasive.Int32.toLarge
      val fromLarge = W.fromLargeWord o LargeWord.toLargeWord
      fun fromLargeInt i =
	 if IntInf.< (i, IntInf.fromInt 0)
	    then raise Overflow
	 else valOf (W.fromString (IntInf.fmt StringCvt.HEX i))
      val fromLargeWord = fromLarge
      val toInt = Pervasive.Int32.fromLarge o W.toLargeInt
      val toIntX = Pervasive.Int32.fromLarge o W.toLargeIntX
      val toLarge = LargeWord.fromLargeWord o W.toLargeWord
      fun toLargeInt w = valOf (IntInf.fromString (W.fmt StringCvt.DEC w))
      val highBit = W.<< (W.fromLargeWord 0w1,
			  Word31.fromInt (Int31.- (W.wordSize, 1)))
      val highBitSub = IntInf.* (IntInf.fromInt 2, toLargeInt highBit)
      fun toLargeIntX w =
	 if W.fromLargeWord 0w0 = W.andb (w, highBit)
	    then toLargeInt w
	 else IntInf.- (toLargeInt w, highBitSub)
      val toLargeWord = toLarge
      val toLargeWordX = LargeWord.fromLargeWord o W.toLargeWordX
      val toLargeX = toLargeWordX
      local
	 (* Bug in SML/NJ -- they use lower instead of upper case. *)
	 val toUpper = Pervasive.String.translate (Char.toString o Char.toUpper)
      in
	 fun fmt r i = toUpper (W.fmt r i)
	 val toString = toUpper o W.toString
      end

      fun ~ (w: word) = fromInt 0 - w
   end

structure Word8 = FixWord (Pervasive.Word8)
structure Word32 =
   struct
      local
	 structure S = FixWord (Pervasive.Word32)
	 open S
	 val highBit: word = 0wx80000000
	 val highBitInt = IntInf.* (IntInf.fromInt 2,
				    Pervasive.IntInf.fromLarge 0x40000000)
      in
	 open S

	 fun fromLargeInt (n: IntInf.int) =
	    if IntInf.< (n, highBitInt)
	       then fromInt (IntInf.toInt n)
	    else
	       highBit + fromInt (IntInf.toInt (IntInf.mod (n, highBitInt)))

	 fun toLargeInt (w: word): IntInf.int =
	    if w < highBit
	       then IntInf.fromInt (toInt w)
	    else IntInf.+ (highBitInt,
			   IntInf.fromInt (toInt
					   (andb (w, notb highBit))))

      end
   end
structure Word64 = FixWord (LargeWord)
structure Word = Word32
structure SysWord = Word32
structure LargeWord = Word64
