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
      val fromInt: Pervasive.Int32.int -> int
      val fromLarge: Pervasive.IntInf.int -> int
      val fromString: string -> int option
      val log2: int -> Pervasive.Int32.int
      val max: int * int -> int
      val maxInt: int option
      val min: int * int -> int
      val minInt: int option
      val mod: int * int -> int
      val pow: int * Pervasive.Int32.int -> int
      val precision: Pervasive.Int32.int option
      val quot: int * int -> int
      val quotRem: int * int -> int * int
      val rem: int * int -> int
      val sameSign: int * int -> bool
      val scan:
	 StringCvt.radix
	 -> (char, 'a) StringCvt.reader
	 -> (int, 'a) StringCvt.reader
      val sign: int -> Pervasive.Int32.int
      val toInt: int -> Pervasive.Int32.int
      val toLarge: int -> Pervasive.IntInf.int
      val toString: int -> string
      val ~ : int -> int
      val orb: int * int -> int
      val xorb: int * int -> int
      val andb: int * int -> int
      val notb: int -> int
      val << : int * Pervasive.Word32.word -> int
      val ~>> : int * Pervasive.Word32.word -> int
   end

structure IntInf: INT_INF =
   struct
      open Pervasive.IntInf

      val fromInt = Pervasive.Int32.toLarge
      val toInt = Pervasive.Int32.fromLarge
      val sign = Pervasive.Int32.fromInt o sign
      val divMod = divMod
      val quotRem = quotRem
      val precision: Pervasive.Int32.int option = NONE
      val log2 = Pervasive.Int32.fromInt o log2
      fun pow (a, b) = Pervasive.IntInf.pow (a, Pervasive.Int32.toInt b)

      local
	fun pow2' (n, w) =
	  if w = 0wx0
	    then n
	  else pow2' (if Pervasive.Word32.andb (0wx1, w) = 0wx1
			then (Pervasive.IntInf.fromInt 2) * n else n,
		      Pervasive.Word32.>> (w, 0wx1))
	fun pow2 w = pow2' (Pervasive.IntInf.fromInt 1, w)
      in
	val (op <<) = fn (a, b) => a * (pow2 b)
	val (op ~>>) = fn (a, b) => a div (pow2 b)
      end
   end

structure LargeInt = IntInf
   
structure Int =
   struct
      open PreInt
      val toLarge = IntInf.fromInt
      val fromLarge = IntInf.toInt
   end

structure Int32 = Int

structure Position = Int
