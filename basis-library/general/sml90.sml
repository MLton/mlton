(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure SML90:> SML90 =
   struct
      type instream = TextIO.instream
      type outstream = TextIO.outstream
      exception Abs = Overflow
      exception Quot = Overflow
      exception Prod = Overflow
      exception Neg = Overflow
      exception Sum = Overflow
      exception Diff = Overflow
      exception Floor = Overflow
      exception Exp = Overflow
      exception Sqrt
      exception Ln
      exception Ord
      exception Mod = Div
      exception Io of string 
      exception Interrupt

      local open Real.Math
      in
	 val sqrt = fn x => if x < 0.0 then raise Sqrt else sqrt x
	 val exp = fn x => let val y = exp x
			   in if Real.isFinite y
				 then y
			      else raise Exp
			   end
	 val ln = fn x => if x > 0.0 then ln x else raise Ln
	 val sin = sin
	 val cos = cos
	 val arctan = atan
      end

      fun ord s =
	 if String.size s = 0
	    then raise Ord
	 else Char.ord(String.sub(s, 0))

      val chr = String.str o Char.chr
      fun explode s = List.map String.str (String.explode s)
      val implode = String.concat
      fun lookahead ins =
	 case TextIO.lookahead ins of
	    NONE => ""
	  | SOME c => str c
	       
      local open TextIO
      in
	 val std_in = stdIn
	 val open_in = openIn
	 val input = inputN
	 val close_in = closeIn
	 val end_of_stream = endOfStream
	 val std_out = stdOut
	 val open_out = openOut
	 val output = output
	 val close_out = closeOut
      end
   end
