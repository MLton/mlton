functor FixWord (W: WORD) =
   struct
      open W

      type t = word
      val wordSize = Pervasive.Int32.fromInt wordSize
      local
	 fun fix (f: t * Word31.word -> t) (w: t, w': Word32.word): t =
	    f (w, Word31.fromLargeWord w')
      in
	 val << = fix <<
	 val >> = fix >>
	 val ~>> = fix ~>>
      end
      val toInt = toLargeInt
      val toIntX = toLargeIntX
      val fromInt = fromLargeInt
      val fromLargeInt = fromInt o Int.fromLarge
      val toLargeInt = Int.toLarge o toInt
      val toLargeIntX: word -> LargeInt.int = Int.toLarge o toIntX
      local
	 (* Bug in SML/NJ -- they use lower instead of upper case. *)
	 val toUpper = Pervasive.String.translate (Char.toString o Char.toUpper)
      in
	 fun fmt r i = toUpper (W.fmt r i)
	 val toString = toUpper o toString
      end
   end

structure Word8 = FixWord (Pervasive.Word8)
structure Word =
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
structure Word32 = Word
structure SysWord = Word
