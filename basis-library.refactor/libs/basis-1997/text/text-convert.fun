(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
