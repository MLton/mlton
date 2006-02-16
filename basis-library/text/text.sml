(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
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
