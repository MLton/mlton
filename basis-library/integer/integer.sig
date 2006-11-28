signature INTEGER_GLOBAL =
   sig
      eqtype int
   end

signature INTEGER =
   sig
      include INTEGER_GLOBAL

      val precision : Int.int option
      val minInt : int option
      val maxInt : int option

      val toLarge: int -> LargeInt.int
      val fromLarge: LargeInt.int -> int
      val toInt: int -> Int.int
      val fromInt: Int.int -> int

      val + : int * int -> int
      val - : int * int -> int
      val * : int * int -> int
      val div: int * int -> int
      val mod: int * int -> int
      val quot: int * int -> int
      val rem: int * int -> int

      val compare: int * int -> order
      val < : int * int -> bool
      val <= : int * int -> bool
      val > : int * int -> bool
      val >= : int * int -> bool

      val ~ : int -> int
      val abs: int -> int
      val min: int * int -> int
      val max: int * int -> int         

      val sign: int -> Int.int
      val sameSign: int * int -> bool

      val fmt: StringCvt.radix -> int -> string
      val toString: int -> string
      val scan: (StringCvt.radix 
                 -> (char, 'a) StringCvt.reader 
                 -> (int, 'a) StringCvt.reader)
      val fromString: string -> int option
   end

signature INTEGER_EXTRA =
   sig
      include INTEGER
      type t = int

      val zero: int
      val one: int

      val precision' : Int.int
      val maxInt' : int
      val minInt' : int

      val +? : int * int -> int
      val *? : int * int -> int
      val -? : int * int -> int
      val ~? : int -> int

      val andb: int * int -> int
      val << : int * Word.word -> int
      val notb: int -> int
      val orb: int * int -> int
      val rol: int * Word.word -> int
      val ror: int * Word.word -> int
      val ~>> : int * Word.word -> int
      val >> : int * Word.word -> int
      val xorb: int * int -> int

      val ltu: int * int -> bool
      val leu: int * int -> bool
      val gtu: int * int -> bool
      val geu: int * int -> bool

      val toLargeInt: int -> LargeInt.int
      val fromLargeInt: LargeInt.int -> int
      val castFromFixedInt: FixedInt.int -> int
      val castToFixedInt: int -> FixedInt.int
      val castFromSysWord: SysWord.word -> int
      val castToSysWord: int -> SysWord.word
   end
