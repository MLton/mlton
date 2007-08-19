(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INT_INF =
   sig
      eqtype int

      val * : int * int -> int
      val + : int * int -> int
      val - : int * int -> int
      val < : int * int -> bool
      val <= : int * int -> bool
      val > : int * int -> bool
      val >= : int * int -> bool
      val abs: int -> int
      val compare: int * int -> order
      val div: int * int -> int
      val divMod: int * int -> int * int
      val fmt: StringCvt.radix -> int -> string
      val fromInt: Pervasive.Int32.int -> int
      val fromLarge: Pervasive.IntInf.int -> int
      val fromString: string -> int option
      val log2: int -> Pervasive.Int32.int
      val max: int * int -> int
      val maxInt: int option
      val min: int * int -> int
      val minInt: int option
      val mod: int * int -> int
      val pow: int * Pervasive.Int32.int -> int
      val precision: Pervasive.Int32.int option
      val quot: int * int -> int
      val quotRem: int * int -> int * int
      val rem: int * int -> int
      val sameSign: int * int -> bool
      val scan:
         StringCvt.radix
         -> (char, 'a) StringCvt.reader
         -> (int, 'a) StringCvt.reader
      val sign: int -> Pervasive.Int32.int
      val toInt: int -> Pervasive.Int32.int
      val toLarge: int -> Pervasive.IntInf.int
      val toString: int -> string
      val ~ : int -> int
      val orb: int * int -> int
      val xorb: int * int -> int
      val andb: int * int -> int
      val notb: int -> int
      val << : int * Pervasive.Word32.word -> int
      val ~>> : int * Pervasive.Word32.word -> int
   end

structure IntInf: INT_INF =
   struct
      open Pervasive.IntInf

      val fromInt = Pervasive.Int32.toLarge
      val toInt = Pervasive.Int32.fromLarge
      val sign = Pervasive.Int32.fromInt o sign
      val precision: Pervasive.Int32.int option = NONE
      val log2 = Pervasive.Int32.fromInt o log2
      fun pow (a, b) = Pervasive.IntInf.pow (a, Pervasive.Int32.toInt b)

      local
        fun pow2 w = 
          if w = 0wx0
            then 1
          else
             let
                val p = pow2 (Pervasive.Word32.>> (w, 0wx1))
                val pp = p * p
             in
                if 0wx1 = Pervasive.Word32.andb (0wx1, w)
                  then 2 * pp
                else pp
             end
      in
        val << = fn (a, b) => a * (pow2 b)
        val ~>> = fn (a, b) => a div (pow2 b)
      end

      local
         (* Bug in SML/NJ -- they use lower instead of upper case. *)
         val toUpper = Pervasive.String.translate (Char.toString o Char.toUpper)
      in
         fun fmt r i = toUpper (Pervasive.IntInf.fmt r i)
         val toString = toUpper o Pervasive.IntInf.toString
      end
   end

structure LargeInt = IntInf

structure Int =
   struct
      open PreInt
      val toLarge = IntInf.fromInt
      val fromLarge = IntInf.toInt
   end

structure Int32 = Int

structure Position = Int

structure Int8 = Int32
structure Int16 = Int32
structure Int64 = Int32
