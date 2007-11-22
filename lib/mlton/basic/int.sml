(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Int:
   sig
      include INTEGER

      val maxInt: t
      val minInt: t
      val roundDownToPowerOfTwo: t -> t
      val roundUpToPowerOfTwo: t -> t
      val toReal: t -> real
   end =
   struct
      structure Int = Pervasive.Int
      structure I = Integer(open Int
                            fun divMod(a, b) = (a div b, a mod b)
                            fun quotRem(a, b) = (quot(a, b), rem(a, b))
                            val toIntInf = Pervasive.IntInf.fromInt)
      open I

      fun roundDownToPowerOfTwo (i: t): t =
         Word.toInt (Word.roundDownToPowerOfTwo (Word.fromInt i))

      fun roundUpToPowerOfTwo (i: t): t =
         let
            val i' = roundDownToPowerOfTwo i
         in
            if i = i'
               then i
            else i' * 2
         end

      type int = t
      val maxInt = valOf Int.maxInt
      val minInt = valOf Int.minInt
      val toReal = Pervasive.Real.fromInt
   end

structure Int32: INTEGER = Int
