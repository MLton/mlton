structure Char: CHAR =
   struct
      open Char0

      val layout = Layout.str o escapeSML

      val fromInt =
	 Trace.trace("Char.fromInt", Layout.str o Int.toString, layout) fromInt

      val isDigit = Trace.trace("Char.isDigit", layout, Bool.layout) isDigit
   end
