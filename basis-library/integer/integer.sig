structure Int =
   struct
      type int = int
   end

structure LargeInt =
   struct
      type int = intInf
   end

signature INTEGER_GLOBAL =
   sig
      eqtype int
   end

signature INTEGER =
   sig
      include INTEGER_GLOBAL

      val toLarge: int -> LargeInt.int 
      val fromLarge: LargeInt.int -> int 
      val toInt: int -> Int.int 
      val fromInt: Int.int -> int 
      val precision: Int.int option 
      val minInt: int option 
      val maxInt: int option 
      val + : int * int -> int 
      val - : int * int -> int 
      val * : int * int -> int 
      val div: int * int -> int 
      val mod: int * int -> int 
      val quot: int * int -> int 
      val rem: int * int -> int
      val compare: int * int -> order 
      val > : int * int -> bool 
      val >= : int * int -> bool 
      val < : int * int -> bool 
      val <= : int * int -> bool 
      val ~ : int -> int 
      val abs: int -> int 
      val min: int * int -> int 
      val max: int * int -> int 
      val sign: int -> Int.int 
      val sameSign: int * int -> bool 
      val fmt: StringCvt.radix -> int -> string 
      val toString: int -> string 
      val scan: StringCvt.radix
	        -> (char, 'a) StringCvt.reader
	        -> (int, 'a) StringCvt.reader
      val fromString: string -> int option 
   end

signature INTEGER_EXTRA =
   sig
      include INTEGER

      val geu: int * int -> bool
      val gtu: int * int -> bool
      val maxInt': int
      val minInt': int
      val power: {base: int, exp: int} -> int
   end

signature INTEGER32_EXTRA =
   sig
      include INTEGER_EXTRA
   end
