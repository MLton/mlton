structure Int =
   struct
      open Int32

      val precision: int option = SOME 32
      val sign = fromInt o sign

      local fun id x = x
      in val toInt = id
	 val fromInt = id
      end

(*       val 'a scan = fn radix =>
 * 	 let
 * 	    val scan: (char, 'a) StringCvt.reader
 * 	       -> (Int31.int, 'a) StringCvt.reader = scan radix
 * 	 in fn reader: (char, 'a) StringCvt.reader =>
 * 	    let val scan: (Int31.int, 'a) StringCvt.reader = scan reader
 * 	    in fn s: 'a =>
 * 	       case scan s of
 * 		  NONE => NONE
 * 		| SOME(n: Int31.int, s) => SOME(OpenInt32.fromInt n, s)
 * 	    end
 * 	 end
 *)

      val toLarge = IntInf.fromInt
      val fromLarge = IntInf.toInt
   end

structure Int32 = Int
