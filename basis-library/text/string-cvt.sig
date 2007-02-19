signature STRING_CVT =
   sig
      datatype radix = BIN | OCT | DEC | HEX

      datatype realfmt =
         SCI of int option 
       | FIX of int option 
       | GEN of int option 
       | EXACT

      type ('a, 'b) reader = 'b -> ('a * 'b) option

      val padLeft: char -> int -> string -> string 
      val padRight: char -> int -> string -> string 

      val splitl: (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a

      val takel: (char -> bool) -> (char, 'a) reader -> 'a -> string 
      val dropl: (char -> bool) -> (char, 'a) reader -> 'a -> 'a 
      val skipWS: (char, 'a) reader -> 'a -> 'a

      type cs
      val scanString:
         ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
   end

signature STRING_CVT_EXTRA =
   sig
      include STRING_CVT

      val radixToInt: radix -> int
      val radixToWord: radix -> word
      val charToDigit: radix -> char -> int option
      val charToWDigit: radix -> char -> word option
      
      (* this exists before Char.isSpace *)
      val isSpace: char -> bool

      (* maps 0...15 to #"0", #"1", ..., #"F" *)
      val digitToChar: int -> char

      (* digitsExact(r, n) reads exactly n digits of radix r *)
      val digitsExact: radix * int -> (char, 'a) reader -> (int, 'a) reader

      (* digitsPlus(r, m) reads between 1 and m digits of radix r *)
      val digitsPlus: radix * int -> (char, 'a) reader -> (int, 'a) reader

      (* digits r reads as many digits of radix r as possible *)
      val digits: radix -> (char, 'a) reader -> (int, 'a) reader
      val wdigits: radix -> (char, 'a) reader -> (word, 'a) reader
   end
