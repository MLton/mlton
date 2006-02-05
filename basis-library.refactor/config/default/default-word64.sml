(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Word = Word64
type word = Word.word

functor WordAddToFromWord(type word
                          val fromWord64 : Word64.word -> word
                          val toWord64 : word -> Word64.word
                          val toWord64X : word -> Word64.word) =
   struct
      val fromWord = fromWord64
      val toWord = toWord64
      val toWordX = toWord64X
   end
