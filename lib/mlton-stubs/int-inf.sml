structure IntInf =
   struct
      open IntInf

      val orb: int * int -> int =
	 fn _ => raise Fail "IntInf.orb"
      val xorb: int * int -> int =
	 fn _ => raise Fail "IntInf.xorb"
      val andb: int * int -> int =
	 fn _ => raise Fail "IntInf.andb"
      val notb: int -> int =
	 fn _ => raise Fail "IntInf.notb"
      val << : int * Word.word -> int =
	 fn _ => raise Fail "IntInf.<<"
      val ~>> : int * Word.word -> int =
	 fn _ => raise Fail "IntInf.~>>"
   end
