(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WORD0 =
   sig
      include PRIM_WORD

      val wordSizeWord': Primitive.Word32.word

      val zero: word
      val one: word

      val div: word * word -> word
      val mod: word * word -> word

      val << : word * Primitive.Word32.word -> word
      val >> : word * Primitive.Word32.word -> word
      val rol : word * Primitive.Word32.word -> word
      val ror : word * Primitive.Word32.word -> word
      val ~>> : word * Primitive.Word32.word -> word

      (* Lowbits or sign extend. *)
      val fromInt8: Primitive.Int8.int -> word
      val fromInt16: Primitive.Int16.int -> word
      val fromInt32: Primitive.Int32.int -> word
      val fromInt64: Primitive.Int64.int -> word

      (* Lowbits or zero extend. *)
      val fromIntZ8: Primitive.Int8.int -> word
      val fromIntZ16: Primitive.Int16.int -> word
      val fromIntZ32: Primitive.Int32.int -> word
      val fromIntZ64: Primitive.Int64.int -> word

      (* Lowbits or zero extend. *)
      val fromWord8: Primitive.Word8.word -> word
      val fromWord16: Primitive.Word16.word -> word
      val fromWord32: Primitive.Word32.word -> word
      val fromWord64: Primitive.Word64.word -> word
         
      (* Lowbits or sign extend. *)
      val fromWordX8: Primitive.Word8.word -> word
      val fromWordX16: Primitive.Word16.word -> word
      val fromWordX32: Primitive.Word32.word -> word
      val fromWordX64: Primitive.Word64.word -> word

      (* Overflow checking, unsigned interp. *)
      val toInt8: word -> Primitive.Int8.int
      val toInt16: word -> Primitive.Int16.int
      val toInt32: word -> Primitive.Int32.int
      val toInt64: word -> Primitive.Int64.int

      (* Overflow checking, signed interp. *)
      val toIntX8: word -> Primitive.Int8.int
      val toIntX16: word -> Primitive.Int16.int
      val toIntX32: word -> Primitive.Int32.int
      val toIntX64: word -> Primitive.Int64.int   

      (* Lowbits or zero extend. *)
      val toWord8: word -> Primitive.Word8.word
      val toWord16: word -> Primitive.Word16.word
      val toWord32: word -> Primitive.Word32.word
      val toWord64: word -> Primitive.Word64.word

      (* Lowbits or sign extend. *)
      val toWordX8: word -> Primitive.Word8.word
      val toWordX16: word -> Primitive.Word16.word
      val toWordX32: word -> Primitive.Word32.word
      val toWordX64: word -> Primitive.Word64.word
   end

functor MkWord0 (W: PRIM_WORD): WORD0 =
   struct

      open W

      val detectOverflow = Primitive.Controls.detectOverflow

      val wordSizeWord' = Primitive.Word32.fromInt32Unsafe wordSize'
      val wordSizeMinusOneWord' = Primitive.Word32.- (wordSizeWord', 0w1)

      val zero: word = fromWord32Unsafe 0w0
      val one: word = fromWord32Unsafe 0w1

      local
         fun make f (w, w') =
            if Primitive.Controls.safe andalso w' = zero
               then raise Div
               else f (w, w')
      in
         val op div = make (op quotUnsafe)
         val op mod = make (op remUnsafe)
      end

      fun << (w, n) =
         if Primitive.Word32.>= (n, wordSizeWord')
            then zero
            else <<? (w, n)
      fun >> (w, n) =
         if Primitive.Word32.>= (n, wordSizeWord')
            then zero
            else >>? (w, n)
      fun ~>> (w, n) =
         if Primitive.Word32.< (n, wordSizeWord')
            then ~>>? (w, n)
            else ~>>? (w, wordSizeMinusOneWord')
      fun rol (w, n) =
         let
            val n = Primitive.Word32.remUnsafe (n, wordSizeWord')
         in
            if n = 0w0
               then w
               else rolUnsafe (w, n)
         end
      fun ror (w, n) =
         let
            val n = Primitive.Word32.remUnsafe (n, wordSizeWord')
         in
            if n = 0w0
               then w
               else rorUnsafe (w, n)
         end

      local
         fun 'a make {fromIntUnsafe: 'a -> word, fromIntZUnsafe: 'a -> word,
                      toIntUnsafe: word -> 'a, toIntXUnsafe: word -> 'a,
                      other : {precision': Primitive.Int32.int,
                               maxInt': 'a,
                               minInt': 'a}} =
            let
               fun toInt w =
                  if detectOverflow
                     andalso Primitive.Int32.>= (wordSize', #precision' other)
                     andalso w > fromIntUnsafe (#maxInt' other)
                     then raise Overflow
                     else toIntUnsafe w
               fun toIntX w =
                  if detectOverflow
                     andalso Primitive.Int32.> (wordSize', #precision' other)
                     andalso fromIntUnsafe (#maxInt' other) < w
                     andalso w < fromIntUnsafe (#minInt' other)
                     then raise Overflow
                     else toIntXUnsafe w
            in
               (fromIntUnsafe,
                fromIntZUnsafe,
                toInt,
                toIntX)
            end
      in
         val (fromInt8, fromIntZ8, toInt8, toIntX8) = 
            make {fromIntUnsafe = fromInt8Unsafe,
                  fromIntZUnsafe = fromIntZ8Unsafe,
                  toIntUnsafe = toInt8Unsafe,
                  toIntXUnsafe = toIntX8Unsafe,
                  other = {precision' = Primitive.Int8.precision',
                           maxInt' = Primitive.Int8.maxInt',
                           minInt' = Primitive.Int8.minInt'}}
         val (fromInt16, fromIntZ16, toInt16, toIntX16) = 
            make {fromIntUnsafe = fromInt16Unsafe,
                  fromIntZUnsafe = fromIntZ16Unsafe,
                  toIntUnsafe = toInt16Unsafe,
                  toIntXUnsafe = toIntX16Unsafe,
                  other = {precision' = Primitive.Int16.precision',
                           maxInt' = Primitive.Int16.maxInt',
                           minInt' = Primitive.Int16.minInt'}}
         val (fromInt32, fromIntZ32, toInt32, toIntX32) = 
            make {fromIntUnsafe = fromInt32Unsafe,
                  fromIntZUnsafe = fromIntZ32Unsafe,
                  toIntUnsafe = toInt32Unsafe,
                  toIntXUnsafe = toIntX32Unsafe,
                  other = {precision' = Primitive.Int32.precision',
                           maxInt' = Primitive.Int32.maxInt',
                           minInt' = Primitive.Int32.minInt'}}
         val (fromInt64, fromIntZ64, toInt64, toIntX64) = 
            make {fromIntUnsafe = fromInt64Unsafe,
                  fromIntZUnsafe = fromIntZ64Unsafe,
                  toIntUnsafe = toInt64Unsafe,
                  toIntXUnsafe = toIntX64Unsafe,
                  other = {precision' = Primitive.Int64.precision',
                           maxInt' = Primitive.Int64.maxInt',
                           minInt' = Primitive.Int64.minInt'}}
      end

      val (fromWord8, fromWordX8, toWord8, toWordX8) =
         (fromWord8Unsafe, fromWordX8Unsafe, toWord8Unsafe, toWordX8Unsafe)   
      val (fromWord16, fromWordX16, toWord16, toWordX16) =
         (fromWord16Unsafe, fromWordX16Unsafe, toWord16Unsafe, toWordX16Unsafe)   
      val (fromWord32, fromWordX32, toWord32, toWordX32) =
         (fromWord32Unsafe, fromWordX32Unsafe, toWord32Unsafe, toWordX32Unsafe)   
      val (fromWord64, fromWordX64, toWord64, toWordX64) =
         (fromWord64Unsafe, fromWordX64Unsafe, toWord64Unsafe, toWordX64Unsafe)   

   end

structure Primitive = struct
open Primitive

structure Word8 = MkWord0 (Primitive.Word8)
structure Word16 = MkWord0 (Primitive.Word16)
structure Word32 = MkWord0 (Primitive.Word32)
structure Word64 = MkWord0 (Primitive.Word64)

end
