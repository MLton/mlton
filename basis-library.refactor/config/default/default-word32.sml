(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Word = Word32
type word = Word.word

functor WordAddToFromWord(type word
                          val fromWord32 : Word32.word -> word
                          val toWord32 : word -> Word32.word
                          val toWord32X : word -> Word32.word) =
   struct
      val fromWord = fromWord32
      val toWord = toWord32
      val toWordX = toWord32X
   end
