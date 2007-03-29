signature INT_INF =
   sig
      include INTEGER

      val divMod: int * int -> int * int
      val quotRem: int * int -> int * int
      val pow: int * Int.int -> int
      val log2: int -> Int.int
      val orb: int * int -> int
      val xorb: int * int -> int
      val andb: int * int -> int
      val notb: int -> int
      val << : int * Word.word -> int
      val ~>> : int * Word.word -> int 
   end

signature INT_INF_EXTRA =
   sig
      include INT_INF
      type t = int

      structure BigWord : WORD
      structure SmallInt : INTEGER

      val areSmall: int * int -> bool
      val gcd: int * int -> int 
      val isSmall: int -> bool
      datatype rep =
         Big of BigWord.word Vector.vector
       | Small of SmallInt.int
      val rep: int -> rep

      val zero: int
      val one: int

      val +? : int * int -> int
      val *? : int * int -> int
      val -? : int * int -> int
      val ~? : int -> int

      val ltu: int * int -> bool
      val leu: int * int -> bool
      val gtu: int * int -> bool
      val geu: int * int -> bool
   end
