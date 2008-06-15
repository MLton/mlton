(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Word: WORD32 =
   struct
      structure Int = Pervasive.Int
      type int = Int.int
      open Pervasive.Word MLton.Word
      structure Z = FixWord (Pervasive.Word)
      open Z

      val equals: t * t -> bool = op =

      fun fromWord8s (f: int -> Word8.t): t =
         let
            fun w (i, shift) =
               Pervasive.Word.<< (Word8.toWord (f i), shift)
         in orb (orb (w (0, 0w0), w (1, 0w8)),
                 orb (w (2, 0w16), w (3, 0w24)))
         end

      val rotateLeft = MLton.Word.rol

      val fromWord = fn x => x
      val toWord = fn x => x
      val toWordX = fn x => x

      val fromIntInf = fromLargeInt
      val toIntInf = toLargeInt
      val toIntInfX = toLargeIntX

      val fromWord8 = Word8.toWord
      val toWord8 = Word8.fromWord

      fun log2 (w: t): t =
         if w = 0w0
            then Error.bug "Word.log2: 0"
         else
            let
               fun loop (n, s, ac): word =
                  if n = 0w1
                     then ac
                  else
                     let
                        val (n, ac) =
                           if n >= << (0w1, s)
                              then (>> (n, s), ac + s)
                           else (n, ac)
                     in
                        loop (n, >> (s, 0w1), ac)
                     end
            in
               loop (w, 0w16, 0w0)
            end

      fun roundDownToPowerOfTwo (w: t) = << (0w1, log2 w)

      fun roundUpToPowerOfTwo w =
         let
            val w' = roundDownToPowerOfTwo w
         in
            if w = w'
               then w
            else w' * 0w2
         end

      structure M = MaxPow2ThatDivides (open Word
                                        type t = word
                                        val equals = op =
                                        val one: t = 0w1
                                        val zero: t = 0w0)
      open M

      fun addCheck (w, w') =
         if w <= ~ 0w1 - w'
            then w + w'
         else raise Overflow
   end
