(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor TextConvert (structure Text: TEXT):
   sig
      structure Char: CHAR
      structure String: STRING_1997
      structure Substring: SUBSTRING_1997
      sharing type Char.char = String.Char.char = Substring.String.Char.char
      sharing type String.string = Substring.String.string
   end =
   struct
      structure Char = Text.Char
      structure String =
	 struct
	    structure Char = Char
	    open Text.String
	 end
      structure Substring =
	 struct
	    structure String = String
	    open Text.Substring
	    val all = full
	 end
   end
