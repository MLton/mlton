(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Text1: TEXT =
   struct
      structure Char = Char1
      structure CharArray = Char1Array
      structure CharArraySlice = Char1ArraySlice
      structure CharVector = Char1Vector
      structure CharVectorSlice = Char1VectorSlice
      structure String = String1
      structure Substring = Substring1
   end
structure Text2: TEXT =
   struct
      structure Char = Char2
      structure CharArray = Char2Array
      structure CharArraySlice = Char2ArraySlice
      structure CharVector = Char2Vector
      structure CharVectorSlice = Char2VectorSlice
      structure String = String2
      structure Substring = Substring2
   end
structure Text4: TEXT =
   struct
      structure Char = Char4
      structure CharArray = Char4Array
      structure CharArraySlice = Char4ArraySlice
      structure CharVector = Char4Vector
      structure CharVectorSlice = Char4VectorSlice
      structure String = String4
      structure Substring = Substring4
   end

structure Text = Text1
structure WideText = Text4
