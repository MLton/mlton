structure Substring =
   struct
      open OpenInt32 Substring

      val full = all
	 
      fun base ss =
	 let val (s, i, j) = Substring.base ss
	 in (s, fromInt i, fromInt j)
	 end

      fun extract(s, i, io) =
	 Substring.extract(s, toInt i, toIntOpt io)

      fun substring(s, i, j) =
	 Substring.substring(s, toInt i, toInt j)

      val triml = triml o toInt
      val trimr = trimr o toInt

      fun slice(s, i, io) =
	 Substring.slice(s, toInt i, toIntOpt io)

      fun sub(ss, i) = Substring.sub(ss, toInt i)

      val size = fromInt o size

      fun splitAt(ss, i) = Substring.splitAt(ss, toInt i)
   end
