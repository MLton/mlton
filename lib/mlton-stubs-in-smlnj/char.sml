structure Char =
   struct
      open Char
      open OpenInt32
	 
      val toCString =
	 fn #"\000" => "\\000"
	  | c => toCString c
      val isCntrl = fn c => isCntrl c orelse c >= #"\127"
      val maxOrd = fromInt maxOrd
      val ord = fromInt o ord
      val chr = chr o toInt
   end
