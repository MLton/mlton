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

      (* Bug in SML/NJ -- they use lower instead of upper case. *)
      val toUpper = Pervasive.String.translate (Char.toString o Char.toUpper)
      fun fmt r i = toUpper (W.fmt r i)
      val toString = toUpper o toString	 
   end

structure Word8 = FixWord (Pervasive.Word8)
structure Word = FixWord (Pervasive.Word32)
structure Word32 = Word
structure SysWord = Word
