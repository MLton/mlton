structure Word32 =
   struct
      type word = word
   end

structure Word = Word32
structure LargeWord = Word32

signature WORD_GLOBAL =
   sig
      eqtype word
   end

signature PRE_WORD =
   sig
      include WORD_GLOBAL

      val * : word * word -> word 
      val + : word * word -> word 
      val - : word * word -> word 
      val < : word * word -> bool 
      val << : word * Word.word -> word 
      val <= : word * word -> bool 
      val > : word * word -> bool 
      val >= : word * word -> bool 
      val >> : word * Word.word -> word 
      val andb: word * word -> word 
      val compare: word * word -> order 
      val div: word * word -> word 
      val fromInt: Int.int -> word 
      val fromLargeWord: LargeWord.word -> word 
      val max: word * word -> word
      val min: word * word -> word 
      val mod: word * word -> word
      val notb: word -> word 
      val orb: word * word -> word 
      val toInt: word -> Int.int 
      val toIntX: word -> Int.int 
      val toLargeWord: word -> LargeWord.word 
      val toLargeWordX: word -> LargeWord.word 
      val wordSize: int 
      val xorb: word * word -> word 
      val ~>> : word * Word.word -> word 
   end

signature WORD =
   sig
      include PRE_WORD
	 
      val fmt: StringCvt.radix -> word -> string 
      val fromLargeInt: LargeInt.int -> word
      val fromString: string -> word option 
      val scan:
	 StringCvt.radix
	 -> (char, 'a) StringCvt.reader
	 -> (word, 'a) StringCvt.reader
      val toLargeInt: word -> LargeInt.int 
      val toLargeIntX: word -> LargeInt.int 
      val toString: word -> string 
   end

signature WORD_EXTRA =
   sig
      include WORD
   end

signature WORD32_EXTRA =
   sig
      include WORD_EXTRA

      val toReal: word -> real
   end
