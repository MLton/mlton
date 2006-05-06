(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INTEGER0 =
   sig
      include PRIM_INTEGER

      val precisionWord': Primitive.Word32.word

      val maxInt: int option
      val minInt: int option

      val zero: int
      val one: int

      val abs: int -> int
      val div: int * int -> int
      val mod: int * int -> int
      val quot: int * int -> int
      val rem: int * int -> int

      val power: {base:int, exp: int} -> int
      val << : int * Primitive.Word32.word -> int
      val rol : int * Primitive.Word32.word -> int
      val ror : int * Primitive.Word32.word -> int
      val ~>> : int * Primitive.Word32.word -> int
      val >> : int * Primitive.Word32.word -> int

      (* Overflow checking, signed interp. *)
      val fromInt8: Primitive.Int8.int -> int
      val fromInt16: Primitive.Int16.int -> int
      val fromInt32: Primitive.Int32.int -> int
      val fromInt64: Primitive.Int64.int -> int

      (* Overflow checking, unsigned interp. *)
      val fromWord8: Primitive.Word8.word -> int
      val fromWord16: Primitive.Word16.word -> int
      val fromWord32: Primitive.Word32.word -> int
      val fromWord64: Primitive.Word64.word -> int

      (* Overflow checking, signed interp. *)
      val fromWord8X: Primitive.Word8.word -> int
      val fromWord16X: Primitive.Word16.word -> int
      val fromWord32X: Primitive.Word32.word -> int
      val fromWord64X: Primitive.Word64.word -> int

      (* Overflow checking. *)
      val toInt8: int -> Primitive.Int8.int
      val toInt16: int -> Primitive.Int16.int
      val toInt32: int -> Primitive.Int32.int
      val toInt64: int -> Primitive.Int64.int

      (* Lowbits or zero extend. *)
      val toWord8: int -> Primitive.Word8.word
      val toWord16: int -> Primitive.Word16.word
      val toWord32: int -> Primitive.Word32.word
      val toWord64: int -> Primitive.Word64.word

      (* Lowbits or sign extend. *)
      val toWord8X: int -> Primitive.Word8.word
      val toWord16X: int -> Primitive.Word16.word
      val toWord32X: int -> Primitive.Word32.word
      val toWord64X: int -> Primitive.Word64.word
   end

functor MkInt0 (I: PRIM_INTEGER): INTEGER0 =
   struct

      open I

      val detectOverflow = Primitive.Controls.detectOverflow

      val precisionWord' = Primitive.Word32.fromInt32Unsafe precision'
      val precisionMinusOneWord' = Primitive.Word32.- (precisionWord', 0w1)

      val maxInt: int option = SOME maxInt'
      val minInt: int option = SOME minInt'

      val zero: int = fromInt32Unsafe 0
      val one: int = fromInt32Unsafe 1

      fun abs (x: int) = if x < zero then ~ x else x

      fun quot (x, y) =
         if Primitive.Controls.safe andalso y = zero
            then raise Div
            else if detectOverflow andalso x = minInt' andalso y = ~one
                    then raise Overflow
                    else quotUnsafe (x, y)
                       
      fun rem (x, y) =
         if Primitive.Controls.safe andalso y = zero
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
                                    else quotUnsafe (x - one, y) -? one
                            else raise Div
            else if y < zero
                    then if detectOverflow andalso x = minInt' andalso y = ~one
                            then raise Overflow
                            else quotUnsafe (x, y)
                    else if y > zero
                            then quotUnsafe (x + one, y) -? one
                            else raise Div
                               
      fun x mod y =
         if x >= zero
            then if y > zero
                    then remUnsafe (x, y)
                    else if y < zero
                            then if x = zero
                                    then zero
                                    else remUnsafe (x - one, y) +? (y + one)
                            else raise Div
            else if y < zero
                    then if x = minInt' andalso y = ~one
                            then zero
                            else remUnsafe (x, y)
                    else if y > zero
                            then remUnsafe (x + one, y) +? (y - one)
                            else raise Div

      fun << (i, n) =
         if Primitive.Word32.>= (n, precisionWord')
            then zero
            else <<? (i, n)
      fun >> (i, n) =
         if Primitive.Word32.>= (n, precisionWord')
            then zero
            else >>? (i, n)
      fun ~>> (i, n) =
         if Primitive.Word32.< (n, precisionWord')
            then ~>>? (i, n)
            else ~>>? (i, precisionMinusOneWord')
      fun rol (i, n) =
         let
            val n = Primitive.Word32.remUnsafe (n, precisionWord')
         in
            if n = 0w0
               then i
               else rolUnsafe (i, n)
         end
      fun ror (i, n) =
         let
            val n = Primitive.Word32.remUnsafe (n, precisionWord')
         in
            if n = 0w0
               then i
               else rorUnsafe (i, n)
         end

      fun power {base, exp} =
         if Primitive.Controls.safe andalso exp < zero
            then raise Primitive.Exn.Fail8 "Int.power"
            else let
                    fun loop (exp, accum) =
                       if exp <= zero
                          then accum
                          else loop (exp - one, base * accum)
                 in loop (exp, one)
                 end

      local
         fun 'a make {fromIntUnsafe: 'a -> int, 
                      toIntUnsafe: int -> 'a,
                      other : {precision': Primitive.Int32.int,
                               maxInt': 'a, 
                               minInt': 'a, 
                               lte : 'a * 'a -> bool}} =
            if detectOverflow andalso
               precision' <> #precision' other
               then if Primitive.Int32.< (precision', #precision' other)
                       then (fn (i : 'a) =>
                             if ((#lte other) (toIntUnsafe minInt', i)
                                 andalso (#lte other) (i, toIntUnsafe maxInt'))
                                then fromIntUnsafe i
                                else raise Overflow, 
                             toIntUnsafe)
                       else (fromIntUnsafe, 
                             fn i =>
                             if (fromIntUnsafe (#minInt' other) <= i
                                 andalso i <= fromIntUnsafe (#maxInt' other))
                                then toIntUnsafe i
                                else raise Overflow)
               else (fromIntUnsafe, toIntUnsafe)
      in
         val (fromInt8, toInt8) = 
            make {fromIntUnsafe = fromInt8Unsafe,
                  toIntUnsafe = toInt8Unsafe,
                  other = {precision' = Primitive.Int8.precision',
                           maxInt' = Primitive.Int8.maxInt',
                           minInt' = Primitive.Int8.minInt',
                           lte = Primitive.Int8.<=}}
         val (fromInt16, toInt16) = 
            make {fromIntUnsafe = fromInt16Unsafe,
                  toIntUnsafe = toInt16Unsafe,
                  other = {precision' = Primitive.Int16.precision',
                           maxInt' = Primitive.Int16.maxInt',
                           minInt' = Primitive.Int16.minInt',
                           lte = Primitive.Int16.<=}}
         val (fromInt32, toInt32) = 
            make {fromIntUnsafe = fromInt32Unsafe,
                  toIntUnsafe = toInt32Unsafe,
                  other = {precision' = Primitive.Int32.precision',
                           maxInt' = Primitive.Int32.maxInt',
                           minInt' = Primitive.Int32.minInt',
                           lte = Primitive.Int32.<=}}
         val (fromInt64, toInt64) = 
            make {fromIntUnsafe = fromInt64Unsafe,
                  toIntUnsafe = toInt64Unsafe,
                  other = {precision' = Primitive.Int64.precision',
                           maxInt' = Primitive.Int64.maxInt',
                           minInt' = Primitive.Int64.minInt',
                           lte = Primitive.Int64.<=}}
      end

      local
         fun 'a make {fromWordUnsafe: 'a -> int, fromWordXUnsafe: 'a -> int,
                      toWordUnsafe: int -> 'a, toWordXUnsafe: int -> 'a,
                      other : {wordSize: Primitive.Int32.int,
                               gt: 'a * 'a -> bool,
                               lt: 'a * 'a -> bool}} =
            let
               fun fromWord w =
                  if detectOverflow
                     andalso Primitive.Int32.>= (#wordSize other, precision')
                     andalso (#gt other) (w, toWordUnsafe maxInt')
                     then raise Overflow
                     else fromWordUnsafe w
               fun fromWordX w =
                  if detectOverflow
                     andalso Primitive.Int32.> (#wordSize other, precision')
                     andalso (#lt other) (toWordUnsafe maxInt', w)
                     andalso (#lt other) (w, toWordUnsafe maxInt')
                     then raise Overflow
                     else fromWordXUnsafe w
            in
               (fromWord, 
                fromWordX, 
                toWordUnsafe, 
                toWordXUnsafe)
            end
      in
         val (fromWord8, fromWord8X, toWord8, toWord8X) =
            make {fromWordUnsafe = fromWord8Unsafe, 
                  fromWordXUnsafe = fromWord8XUnsafe,
                  toWordUnsafe = toWord8Unsafe,
                  toWordXUnsafe =toWord8XUnsafe,
                  other = {wordSize = Primitive.Word8.wordSize,
                           lt = Primitive.Word8.<,
                           gt = Primitive.Word8.>}}
         val (fromWord16, fromWord16X, toWord16, toWord16X) =
            make {fromWordUnsafe = fromWord16Unsafe, 
                  fromWordXUnsafe = fromWord16XUnsafe,
                  toWordUnsafe = toWord16Unsafe,
                  toWordXUnsafe =toWord16XUnsafe,
                  other = {wordSize = Primitive.Word16.wordSize,
                           lt = Primitive.Word16.<,
                           gt = Primitive.Word16.>}}
         val (fromWord32, fromWord32X, toWord32, toWord32X) =
            make {fromWordUnsafe = fromWord32Unsafe, 
                  fromWordXUnsafe = fromWord32XUnsafe,
                  toWordUnsafe = toWord32Unsafe,
                  toWordXUnsafe =toWord32XUnsafe,
                  other = {wordSize = Primitive.Word32.wordSize,
                           lt = Primitive.Word32.<,
                           gt = Primitive.Word32.>}}
         val (fromWord64, fromWord64X, toWord64, toWord64X) =
            make {fromWordUnsafe = fromWord64Unsafe, 
                  fromWordXUnsafe = fromWord64XUnsafe,
                  toWordUnsafe = toWord64Unsafe,
                  toWordXUnsafe =toWord64XUnsafe,
                  other = {wordSize = Primitive.Word64.wordSize,
                           lt = Primitive.Word64.<,
                           gt = Primitive.Word64.>}}
      end
   
   end

structure Primitive = struct
open Primitive

structure Int8 = MkInt0 (Primitive.Int8)
structure Int16 = MkInt0 (Primitive.Int16)
structure Int32 = MkInt0 (Primitive.Int32)
structure Int64 = MkInt0 (Primitive.Int64)

end
