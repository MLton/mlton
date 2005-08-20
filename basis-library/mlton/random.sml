(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonRandom: MLTON_RANDOM =
   struct
      (* Uses /dev/random and /dev/urandom to get a random word.
       * If they can't be read from, return NONE.
       *)
      local
         fun make (file, name) =
            let
               val buf = Word8Array.array (4, 0w0)
            in
               fn () =>
               (let
                   val fd =
                      let
                         open Posix.FileSys
                      in
                         openf (file, O_RDONLY, O.flags [])
                      end
                   fun loop rem =
                      let
                         val n = Posix.IO.readArr (fd,
                                                   Word8ArraySlice.slice
                                                   (buf, 4 - rem, SOME rem))
                         val _ = if n = 0
                                    then (Posix.IO.close fd; raise Fail name)
                                 else ()
                         val rem = rem - n
                      in
                         if rem = 0
                            then ()
                         else loop rem
                      end
                   val _ = loop 4
                   val _ = Posix.IO.close fd
                in
                   SOME (Word.fromLarge (PackWord32Little.subArr (buf, 0)))
                end
                   handle OS.SysErr _ => NONE)
            end
      in
         val seed = make ("/dev/random", "Random.seed")
         val useed = make ("/dev/urandom", "Random.useed")
      end

      local
         open Word
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
