structure TextIO =
   struct
      open OpenInt32 TextIO

      fun inputLine ins =
	 case TextIO.inputLine ins of
	    "" => NONE
	  | s => SOME s
      fun inputN (ins, n) = TextIO.inputN (ins, toInt n)
      fun canInput (ins, n) = TextIO.canInput (ins, toInt n)
   end
