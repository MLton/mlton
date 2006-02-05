(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure TextPrimIO : PRIM_IO
                       where type array = CharArray.array
                       where type vector = CharVector.vector
                       where type elem = Char.char =
  PrimIO (structure Vector = CharVector
          structure VectorSlice = CharVectorSlice
          structure Array = CharArray
          structure ArraySlice = CharArraySlice
          type pos = Position.int
          val compare = Position.compare
          val someElem = #"\000": Char.char)

