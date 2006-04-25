signature INTEGER_GLOBAL =
   sig
      eqtype int
   end

signature PRE_INTEGER =
   sig
      include INTEGER_GLOBAL

      val toLarge: int -> LargeInt.int
      val fromLarge: LargeInt.int -> int
      val toInt: int -> Int.int
      val fromInt: Int.int -> int

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
      val < : int * int -> bool
      val <= : int * int -> bool
      val > : int * int -> bool
      val >= : int * int -> bool
         
      val ~ : int -> int
      val abs: int -> int
      val min: int * int -> int
      val max: int * int -> int         
   end

signature PRE_INTEGER_EXTRA =
   sig
      include PRE_INTEGER

      val zero: int
      val one: int

      val precision' : Primitive.Int32.int
      val precisionWord' : Primitive.Word32.word

      val maxInt' : int
      val minInt' : int

      val *? : int * int -> int
      val +? : int * int -> int
      val -? : int * int -> int
      val ~? : int -> int
      val power: {base: int, exp: int} -> int

      val andb: int * int -> int
      val << : int * Primitive.Word32.word -> int
      val notb: int -> int
      val orb: int * int -> int
      val rol: int * Primitive.Word32.word -> int
      val ror: int * Primitive.Word32.word -> int
      val ~>> : int * Primitive.Word32.word -> int
      val >> : int * Primitive.Word32.word -> int
      val xorb: int * int -> int

      val ltu: int * int -> bool
      val leu: int * int -> bool
      val gtu: int * int -> bool
      val geu: int * int -> bool
   end

signature INTEGER =
   sig
      include PRE_INTEGER

      val precision: Int.int option
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
   end
