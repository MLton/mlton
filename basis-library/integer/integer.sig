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

      val * : int * int -> int 
      val + : int * int -> int 
      val - : int * int -> int 
      val < : int * int -> bool 
      val <= : int * int -> bool 
      val > : int * int -> bool 
      val >= : int * int -> bool 
      val abs: int -> int 
      val compare: int * int -> order 
      val div: int * int -> int 
      val fmt: StringCvt.radix -> int -> string 
      val fromInt: Int.int -> int 
      val fromLarge: LargeInt.int -> int 
      val fromString: string -> int option 
      val max: int * int -> int 
      val maxInt: int option 
      val min: int * int -> int 
      val minInt: int option 
      val mod: int * int -> int 
      val precision: Int.int option 
      val quot: int * int -> int 
      val rem: int * int -> int
      val sameSign: int * int -> bool 
      val scan: StringCvt.radix
	 -> (char, 'a) StringCvt.reader
	 -> (int, 'a) StringCvt.reader
      val sign: int -> Int.int 
      val toInt: int -> Int.int 
      val toLarge: int -> LargeInt.int 
      val toString: int -> string 
      val ~ : int -> int 
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
