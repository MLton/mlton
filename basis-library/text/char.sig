signature CHAR_GLOBAL =
   sig
      eqtype char

      val ord: char -> int 
      val chr: int -> char 
   end

signature CHAR =
   sig
      include CHAR_GLOBAL

      eqtype string

      val < : char * char -> bool 
      val <= : char * char -> bool 
      val > : char * char -> bool 
      val >= : char * char -> bool 
      val compare: char * char -> order 
      val contains: string -> char -> bool 
      val fromCString: string -> char option
      val fromString: string -> char option 
      val isAlpha: char -> bool 
      val isAlphaNum: char -> bool 
      val isAscii: char -> bool 
      val isCntrl: char -> bool 
      val isDigit: char -> bool 
      val isGraph: char -> bool 
      val isHexDigit: char -> bool 
      val isLower: char -> bool 
      val isPrint: char -> bool 
      val isPunct: char -> bool 
      val isSpace: char -> bool 
      val isUpper: char -> bool 
      val maxChar: char 
      val maxOrd: int 
      val minChar: char 
      val notContains: string -> char -> bool 
      val pred: char -> char 
      val scan: (char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
      val succ: char -> char 
      val toCString: char -> string
      val toLower: char -> char 
      val toString: char -> string 
      val toUpper: char -> char 
   end

signature CHAR_EXTRA =
   sig
      include CHAR

      val scanC: (char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
   end
