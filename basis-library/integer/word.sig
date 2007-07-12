signature WORD_GLOBAL =
   sig
      eqtype word
   end

signature WORD =
   sig
      include WORD_GLOBAL

      val wordSize: Int.int

      val toLarge: word -> LargeWord.word
      val toLargeX: word -> LargeWord.word
      val toLargeWord: word -> LargeWord.word
      val toLargeWordX: word -> LargeWord.word
      val fromLarge: LargeWord.word -> word
      val fromLargeWord: LargeWord.word -> word
      val toLargeInt: word -> LargeInt.int
      val toLargeIntX: word -> LargeInt.int
      val fromLargeInt: LargeInt.int -> word
      val toInt: word -> int
      val toIntX: word -> int
      val fromInt: int -> word

      val + : word * word -> word
      val - : word * word -> word
      val * : word * word -> word
      val div: word * word -> word
      val mod: word * word -> word

      val andb: word * word -> word
      val << : word * Word.word -> word
      val notb: word -> word
      val orb: word * word -> word
      val ~>> : word * Word.word -> word
      val >> : word * Word.word -> word
      val xorb: word * word -> word

      val compare: word * word -> order
      val < : word * word -> bool
      val <= : word * word -> bool
      val > : word * word -> bool
      val >= : word * word -> bool

      val ~ : word -> word
      val min: word * word -> word
      val max: word * word -> word

      val fmt: StringCvt.radix -> word -> string
      val toString: word -> string
      val scan: (StringCvt.radix
                 -> (char, 'a) StringCvt.reader
                 -> (word, 'a) StringCvt.reader)
      val fromString: string -> word option
   end

signature WORD_EXTRA =
   sig
      include WORD
      type t = word

      val zero: word
      val one: word

      val maxWord' : word

      val toWord: word -> Word.word
      val toWordX: word -> Word.word
      val fromWord: Word.word -> word

      val bswap: word -> word
      val rol: word * Word.word -> word
      val ror: word * Word.word -> word
      val log2 : word -> Primitive.Int32.int

      val castFromFixedInt: FixedInt.int -> word
      val castToFixedInt: word -> FixedInt.int
      val castFromSysWord: SysWord.word -> word
      val castToSysWord: word -> SysWord.word
   end
