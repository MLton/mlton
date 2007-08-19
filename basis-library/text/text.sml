(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Text: TEXT =
   struct
      structure Char = Char
      structure CharArray = CharArray
      structure CharArraySlice = CharArraySlice
      structure CharVector = CharVector
      structure CharVectorSlice = CharVectorSlice
      structure String = String
      structure Substring = Substring
   end

structure WideText: TEXT =
   struct
      structure Char = WideChar
      structure CharArray = WideCharArray
      structure CharArraySlice = WideCharArraySlice
      structure CharVector = WideCharVector
      structure CharVectorSlice = WideCharVectorSlice
      structure String = WideString
      structure Substring = WideSubstring
   end
