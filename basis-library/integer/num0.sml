(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MKNUM0_ARG =
   sig
      structure Int: PRIM_INTEGER
      structure Word: PRIM_WORD
      val idFromIntToWord: Int.int -> Word.word
      val idFromWordToInt: Word.word -> Int.int
   end
signature PRIM_INTEGER =
   sig
      include PRIM_INTEGER

      val maxInt': int
      val minInt': int
      val maxInt: int option
      val minInt: int option

      val zero: int
      val one: int

      val abs: int -> int
      val div: int * int -> int
      val mod: int * int -> int
      val quot: int * int -> int
      val rem: int * int -> int

      val ltu: int * int -> bool
      val leu: int * int -> bool
      val gtu: int * int -> bool
      val geu: int * int -> bool

      val andb : int * int -> int
      val <<? : int * Primitive.Word32.word -> int
      val notb : int -> int
      val orb : int * int -> int
      val rolUnsafe : int * Primitive.Word32.word -> int
      val rorUnsafe : int * Primitive.Word32.word -> int
      val ~>>? : int * Primitive.Word32.word -> int
      val >>? : int * Primitive.Word32.word -> int
      val xorb : int * int -> int

      val power: {base:int, exp: int} -> int
      val log2: int -> Primitive.Int32.int
      val log2Word: int -> Primitive.Word32.word
   end
signature PRIM_WORD =
   sig
      include PRIM_WORD

      val zero: word
      val one: word

      val maxWord': word

      val div: word * word -> word
      val mod: word * word -> word

      val log2: word -> Primitive.Int32.int
      val log2Word: word -> Primitive.Word32.word
   end

functor MkNum0 (S: MKNUM0_ARG): sig
                                   structure Int: PRIM_INTEGER
                                   structure Word: PRIM_WORD
                                end =
   struct
      open S

      val _ =
         if Int.sizeInBits <> Word.sizeInBits
            orelse Int.sizeInBitsWord <> Word.sizeInBitsWord
            then raise Primitive.Exn.Fail8 "MkNum0: Int.sizeInBits <> Word.sizeInBits"
            else ()

      structure Word =
         struct
            open Word

            val zero = zextdFromWord32 0w0
            val one = zextdFromWord32 0w1

            val maxWord' = notb zero

            local
               fun make f (w, w') =
                  if Primitive.Controls.safe andalso w' = zero
                     then raise Div
                     else f (w, w')
            in
               val op div = make (op quotUnsafe)
               val op mod = make (op remUnsafe)
            end

            fun log2Word w =
               let
                  fun loop (n, s, acc) =
                     if n = one
                        then acc
                        else let
                                val (n, acc) =
                                   if n >= <<? (one, s)
                                      then (>>? (n, s), Primitive.Word32.+ (acc, s))
                                      else (n, acc)
                             in
                                loop (n, Primitive.Word32.>>? (s, 0w1), acc)
                             end
               in
                  if Primitive.Controls.safe andalso w = zero
                     then raise Domain
                     else loop (w, Primitive.Word32.>>? (sizeInBitsWord, 0w1), 0w0)
               end
            fun log2 w = Primitive.IntWordConv.zextdFromWord32ToInt32 (log2Word w)
         end

      structure Int = 
         struct
            open Int

            val zero = zextdFromInt32 0
            val one = zextdFromInt32 1

            local
               fun makeBinop f =
                  fn (x: int, y: int) =>
                  idFromWordToInt
                  (f (idFromIntToWord x, idFromIntToWord y))
               fun makeUnop f =
                  fn (x: int) =>
                  idFromWordToInt
                  (f (idFromIntToWord x))
               fun makeShop f =
                  fn (x: int, w: Primitive.Word32.word) =>
                  idFromWordToInt
                  (f (idFromIntToWord x, w))
            in
               val andb = makeBinop Word.andb
               val <<? = makeShop Word.<<?
               val notb = makeUnop Word.notb
               val orb = makeBinop Word.orb
               val rolUnsafe = makeShop Word.rolUnsafe
               val rorUnsafe = makeShop Word.rorUnsafe
               val ~>>? = makeShop Word.~>>?
               val >>? = makeShop Word.>>?
               val xorb = makeBinop Word.xorb
            end
            fun log2 i = Word.log2 (idFromIntToWord i)
            fun log2Word i = Word.log2Word (idFromIntToWord i)

            val minInt' = <<? (one, Primitive.Word32.- (sizeInBitsWord, 0w1))
            val maxInt' = >>? (notb zero, 0w1)
            val minInt = SOME minInt'
            val maxInt = SOME maxInt'

            fun abs (x: int) = if x < zero then ~ x else x

            fun quot (x, y) =
               if Primitive.Controls.safe 
                  andalso y = zero
                  then raise Div
                  else if (Primitive.Controls.detectOverflow
                           orelse Primitive.Controls.safe)
                          andalso x = minInt' andalso y = ~one
                          then if Primitive.Controls.detectOverflow 
                                  then raise Overflow
                                  else minInt'
                          else quotUnsafe (x, y)

            fun rem (x, y) =
               if Primitive.Controls.safe 
                  andalso y = zero
                  then raise Div
                  else if x = minInt' andalso y = ~one
                          then zero
                          else remUnsafe (x, y)

            fun x div y =
               if x >= zero
                  then if y > zero
                          then quotUnsafe (x, y)
                          else if y < zero
                                  then if x = zero
                                          then zero
                                          else quotUnsafe (x -? one, y) -? one
                                  else raise Div
                  else if y < zero
                          then if (Primitive.Controls.detectOverflow
                                   orelse Primitive.Controls.safe)
                                  andalso x = minInt' andalso y = ~one
                                  then if Primitive.Controls.detectOverflow 
                                          then raise Overflow
                                          else minInt'
                                  else quotUnsafe (x, y)
                          else if y > zero
                                  then quotUnsafe (x +? one, y) -? one
                                  else raise Div

            fun x mod y =
               if x >= zero
                  then if y > zero
                          then remUnsafe (x, y)
                          else if y < zero
                                  then if x = zero
                                          then zero
                                          else remUnsafe (x -? one, y) +? (y + one)
                                  else raise Div
                  else if y < zero
                          then if x = minInt' andalso y = ~one
                                  then zero
                                  else remUnsafe (x, y)
                          else if y > zero
                                  then remUnsafe (x +? one, y) +? (y -? one)
                                  else raise Div

            local
               structure S = 
                  UnsignedIntegralComparisons
                  (type int = int
                   type word = Word.word
                   val idFromIntToWord = idFromIntToWord
                   val op < = Word.<)
            in
               open S
            end

            fun power {base, exp} =
               if Primitive.Controls.safe 
                  andalso exp < zero
                  then raise Primitive.Exn.Fail8 "Int.power"
                  else let
                          fun loop (exp, accum) =
                             if exp <= zero
                                then accum
                                else loop (exp - one, base * accum)
                       in loop (exp, one)
                       end
         end

   end

structure Primitive = struct
open Primitive

local
   structure S =
      MkNum0 (structure Int = Int8
              structure Word = Word8
              val idFromIntToWord = IntWordConv.idFromInt8ToWord8
              val idFromWordToInt = IntWordConv.idFromWord8ToInt8)
in
   structure Int8 : PRIM_INTEGER = S.Int
   structure Word8 : PRIM_WORD = S.Word
end
local
   structure S =
      MkNum0 (structure Int = Int16
              structure Word = Word16
              val idFromIntToWord = IntWordConv.idFromInt16ToWord16
              val idFromWordToInt = IntWordConv.idFromWord16ToInt16)
in
   structure Int16 : PRIM_INTEGER = S.Int
   structure Word16 : PRIM_WORD = S.Word
end
local
   structure S =
      MkNum0 (structure Int = Int32
              structure Word = Word32
              val idFromIntToWord = IntWordConv.idFromInt32ToWord32
              val idFromWordToInt = IntWordConv.idFromWord32ToInt32)
in
   structure Int32 : PRIM_INTEGER = S.Int
   structure Word32 : PRIM_WORD = S.Word
end
local
   structure S =
      MkNum0 (structure Int = Int64
              structure Word = Word64
              val idFromIntToWord = IntWordConv.idFromInt64ToWord64
              val idFromWordToInt = IntWordConv.idFromWord64ToInt64)
in
   structure Int64 : PRIM_INTEGER = S.Int
   structure Word64 : PRIM_WORD = S.Word
end

end
