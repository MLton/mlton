structure Word =
   struct
      type word = word
   end

structure LargeWord = 
   struct
      type word = word64
   end

signature WORD_GLOBAL =
   sig
      eqtype word
   end

signature PRE_WORD =
   sig
      include WORD_GLOBAL

      val wordSize: int 
      val toLarge: word -> LargeWord.word
      val toLargeX: word -> LargeWord.word
      val fromLarge: LargeWord.word -> word 
      val toInt: word -> Int.int 
      val toIntX: word -> Int.int 
      val fromInt: Int.int -> word 
      val orb: word * word -> word 
      val xorb: word * word -> word 
      val andb: word * word -> word 
      val notb: word -> word 
      val << : word * Word.word -> word 
      val >> : word * Word.word -> word 
      val ~>> : word * Word.word -> word 
      val + : word * word -> word 
      val - : word * word -> word 
      val * : word * word -> word 
      val div: word * word -> word 
      val mod: word * word -> word
      val ~ : word -> word
      val < : word * word -> bool 
      val > : word * word -> bool 
      val >= : word * word -> bool 
      val <= : word * word -> bool 
   end
signature PRE_WORD_EXTRA =
   sig
      include PRE_WORD
   end

signature WORD =
   sig
      include PRE_WORD
	 
      val toLargeWord: word -> LargeWord.word 
      val toLargeWordX: word -> LargeWord.word 
      val fromLargeWord: LargeWord.word -> word 
      val compare: word * word -> order 
      val min: word * word -> word 
      val max: word * word -> word
      val toLargeInt: word -> LargeInt.int 
      val toLargeIntX: word -> LargeInt.int 
      val fromLargeInt: LargeInt.int -> word
      val fmt: StringCvt.radix -> word -> string 
      val toString: word -> string 
      val scan: StringCvt.radix
	        -> (char, 'a) StringCvt.reader
	        -> (word, 'a) StringCvt.reader
      val fromString: string -> word option 
   end

signature WORD_EXTRA =
   sig
      include WORD
      (* include PRE_WORD_EXTRA *)
   end

signature WORD32_EXTRA =
   sig
      include WORD_EXTRA

(*      val toReal: word -> real *)
   end
