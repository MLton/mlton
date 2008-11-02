(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IntInf: INTEGER = Integer (open Pervasive.IntInf
                                     fun toIntInf x = x)

structure IntInf: INT_INF =
   struct
      open IntInf

      val hash = let
         val prime =
             (Word.toIntInf o Word.~ o Word.fromInt)
              (case Word.wordSize of
                  8 => 5
                | 16 => 15
                | 32 => 5
                | 64 => 59
                | 128 => 159
                | _ => Error.bug "Unknown Word.wordSize")
      in
         fn i => Word.fromIntInf (i mod prime)
      end

      local
         open Pervasive.IntInf
      in
         val andb = andb
         val log2 = log2
         val notb = notb
         val orb = orb
         val xorb = xorb
         val op ~>> = ~>>
         val op << = <<
      end

      structure M = MaxPow2ThatDivides (open IntInf
                                        val andb = andb
                                        val orb = orb
                                        val << = <<
                                        val >> = ~>>)
      open M
   end

structure LargeInt = IntInf
