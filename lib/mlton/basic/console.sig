(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature CONSOLE =
   sig
      structure Background:
	 sig
	    datatype t =
	       Black | Red | Green | Brown | Blue | Magenta | Cyan | Gray
	 end

      structure Foreground:
	 sig
	    datatype t =
	       DarkGray | BrightRed | BrightGreen | Yellow | BrightBlue
	     | BrightMagenta | BrightCyan | White
	 end
	 
      structure CharRendition:
	 sig
	    datatype t =
	       Default (* Normal, UnderlineOff, ReverseVideoOf,f BlinkOff *)
	     | Bold
	     | Dim
	     | Normal
	     | UnderlineOn
	     | UnderlineOff
	     | UnderlineOnDefaultForeground
	     | UnderlineOffDefaultForeground
	     | BlinkOn
	     | BlinkOff
	     | ReverseVideoOn
	     | ReverseVideoOff
	     | Foreground of Foreground.t
	     | Background of Background.t

	    val set: t list -> string
	 end

      val moveToColumn: int -> string
   end
