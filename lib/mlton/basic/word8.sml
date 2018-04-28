(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Use fromLargeWord instead of fromLarge so we can compile this code
 * with older MLtons that don't have fromLarge.
 *)
functor FixWord (W: PERVASIVE_WORD) =
   struct
      type t = W.word

      local
         structure LargeWord = Pervasive.LargeWord
         structure Word = Pervasive.Word
         structure Word8 = Pervasive.Word8
      in
         fun format (w, f) = W.fmt f w
         val fromChar = W.fromLargeWord o Word8.toLargeWord o Byte.charToByte
         val fromIntInf = W.fromLargeInt
         val fromLarge = W.fromLargeWord o LargeWord.toLargeWord
         val fromWord = W.fromLargeWord o Word.toLargeWord
         val layout = Layout.str o W.toString
         fun nthBitIsSet (w: t, n: int): bool =
            W.fromInt 1 = W.andb (W.fromInt 1, W.>> (w, Word.fromInt n))
         val toChar = Byte.byteToChar o Word8.fromLargeWord o W.toLargeWord
         val toIntInf = W.toLargeInt
         val toIntInfX = W.toLargeIntX
         val toLarge = LargeWord.fromLargeWord o W.toLargeWord
         val toLargeX = LargeWord.fromLargeWord o W.toLargeWordX
         val toWord = Word.fromLargeWord o W.toLargeWord
         val toWordX = Word.fromLargeWord o W.toLargeWordX
      end
   end

structure Word8:
   sig
      include WORD

      val stringToVector: string -> t vector
      val vectorToString: t vector -> string
   end =
   struct
      open Pervasive.Word8
      structure Z = FixWord (Pervasive.Word8)
      open Z

      val equals: t * t -> bool = op =

      fun vectorToString v =
         CharVector.tabulate (Pervasive.Vector.length v, fn i =>
                              toChar (Pervasive.Vector.sub (v, i)))

      fun stringToVector s =
         Pervasive.Vector.tabulate (Pervasive.String.size s, fn i =>
                                    fromChar (Pervasive.String.sub (s, i)))
   end
