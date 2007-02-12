signature PRE_CHAR =
   sig
      eqtype char
      eqtype string
      
      val chrUnsafe: int -> char
      val ord: char -> int
      
      val fromChar: Char.char -> char
      
      val minChar : char
      val maxChar : char
      val numChars : int
      
      val compare: char * char -> order
      val <  : char * char -> bool
      val <= : char * char -> bool
      val >  : char * char -> bool
      val >= : char * char -> bool
   end
