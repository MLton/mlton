structure Word =
   struct
      type word = word
   end

structure LargeWord = 
   struct
      type word = Primitive.Word64.word
   end

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
      val div: word * word -> word 
      val fromInt: Int.int -> word 
      val fromLarge: LargeWord.word -> word 
      val mod: word * word -> word
      val notb: word -> word 
      val orb: word * word -> word 
      val toInt: word -> Int.int 
      val toIntX: word -> Int.int 
      val toLarge: word -> LargeWord.word
      val toLargeX: word -> LargeWord.word
      val wordSize: int 
      val xorb: word * word -> word 
      val ~ : word -> word
      val ~>> : word * Word.word -> word 
   end
signature PRE_WORD_EXTRA =
   sig
      include PRE_WORD
   end

signature WORD =
   sig
      include PRE_WORD
         
      val compare: word * word -> order 
      val fmt: StringCvt.radix -> word -> string 
      val fromLargeInt: LargeInt.int -> word
      val fromLargeWord: LargeWord.word -> word 
      val fromString: string -> word option 
      val max: word * word -> word
      val min: word * word -> word 
      val scan: (StringCvt.radix
                 -> (char, 'a) StringCvt.reader
                 -> (word, 'a) StringCvt.reader)
      val toLargeInt: word -> LargeInt.int 
      val toLargeIntX: word -> LargeInt.int 
      val toLargeWord: word -> LargeWord.word 
      val toLargeWordX: word -> LargeWord.word 
      val toString: word -> string 
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
