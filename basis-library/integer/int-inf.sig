signature INT_INF =
   sig
      eqtype int

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
      val divMod: int * int -> int * int
      val fmt: StringCvt.radix -> int -> string
      val fromInt: Int.int -> int
      val fromLarge: LargeInt.int -> int
      val fromString: string -> int option
      val max: int * int -> int
      val maxInt: int option
      val min: int * int -> int
      val minInt: int option
      val mod: int * int -> int
      val pow: int * Int.int -> int
      val precision: Int.int option
      val quot: int * int -> int
      val quotRem: int * int -> int * int
      val rem: int * int -> int
      val sameSign: int * int -> bool
      val scan:
	 StringCvt.radix
	 -> (char, 'a) StringCvt.reader
	 -> (int, 'a) StringCvt.reader
      val sign: int -> Int.int
      val toInt: int -> Int.int
      val toLarge: int -> LargeInt.int
      val toString: int -> string
      val ~ : int -> int
(*      val log2: int -> Int.int
 *      val orb: int * int -> int
 *      val xorb: int * int -> int
 *      val andb: int * int -> int
 *      val notb: int -> int
 *      val << : int * Word.word -> int
 *      val ~>> : int * Word.word -> int
 *)
   end

signature INT_INF_EXTRA =
   sig
      include INT_INF
	 
      val bigIntConstant: Int.int -> int
      val gcd: int * int -> int 
      datatype rep =
	 Small of Word.word
       | Big of Word.word Vector.vector
      val rep: int -> rep
      val size: int -> Int.int
   end
