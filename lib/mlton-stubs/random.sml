(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Random: MLTON_RANDOM = 
   struct
      fun seed _ = SOME (0w13: Word32.word)
      fun useed _ = SOME (0w13: Word32.word)
      local
         val seed: word ref = ref 0w13
      in
         (* From page 284 of Numerical Recipes in C. *)
         fun rand (): word =
            let
               val res = 0w1664525 * !seed + 0w1013904223
               val _ = seed := res
            in
               res
            end

         fun srand (w: word): unit = seed := w
      end

      structure String =
         struct
            open String

            val tabulate = CharVector.tabulate
         end

      local
         val chars =
            "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         val numChars = String.size chars
         val refresh =
            let
               val numChars = IntInf.fromInt numChars
               fun loop (i: IntInf.int, c: int): int =
                  if IntInf.< (i, numChars)
                     then c
                  else loop (IntInf.div (i, numChars), c + 1)
            in
               loop (IntInf.pow (2, Word.wordSize), 0)
            end
         val r: word ref = ref 0w0
         val count: int ref = ref refresh
         val numChars = Word.fromInt numChars
      in
         fun alphaNumChar (): char =
            let
               val n = !count
               val _ = if n = refresh
                          then (r := rand ()
                                ; count := 1)
                       else (count := n + 1)
               val w = !r
               val c = String.sub (chars, Word.toInt (Word.mod (w, numChars)))
               val _ = r := Word.div (w, numChars)
            in
               c
            end
      end

      fun alphaNumString (length: int): string =
         String.tabulate (length, fn _ => alphaNumChar ())
   end
