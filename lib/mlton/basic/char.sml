(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Char: CHAR =
   struct
      open Char0

      val layout = Layout.str o escapeSML

      val fromInt =
	 Trace.trace("Char.fromInt", Layout.str o Int.toString, layout) fromInt

      val isDigit = Trace.trace("Char.isDigit", layout, Bool.layout) isDigit
   end
