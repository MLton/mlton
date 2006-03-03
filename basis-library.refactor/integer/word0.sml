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

      val wordSizeWord: Primitive.Word32.word

      val zero: word
      val one: word

      val div: word * word -> word
      val mod: word * word -> word

      val << : word * Primitive.Word32.word -> word
      val >> : word * Primitive.Word32.word -> word
      val rol : word * Primitive.Word32.word -> word
      val ror : word * Primitive.Word32.word -> word
      val ~>> : word * Primitive.Word32.word -> word
      val log2 : word -> Primitive.Int32.int

      (* Lowbits or sign extend. *)
      val fromInt8: Primitive.Int8.int -> word
      val fromInt16: Primitive.Int16.int -> word
      val fromInt32: Primitive.Int32.int -> word
      val fromInt64: Primitive.Int64.int -> word

(*
      (* Lowbits or zero extend. *)
      val fromInt8Z: Primitive.Int8.int -> word
      val fromInt16Z: Primitive.Int16.int -> word
      val fromInt32Z: Primitive.Int32.int -> word
      val fromInt64Z: Primitive.Int64.int -> word
*)

      (* Lowbits or zero extend. *)
      val fromWord8: Primitive.Word8.word -> word
      val fromWord16: Primitive.Word16.word -> word
      val fromWord32: Primitive.Word32.word -> word
      val fromWord64: Primitive.Word64.word -> word
         
      (* Lowbits or sign extend. *)
      val fromWord8X: Primitive.Word8.word -> word
      val fromWord16X: Primitive.Word16.word -> word
      val fromWord32X: Primitive.Word32.word -> word
      val fromWord64X: Primitive.Word64.word -> word

      (* Overflow checking, unsigned interp. *)
      val toInt8: word -> Primitive.Int8.int
      val toInt16: word -> Primitive.Int16.int
      val toInt32: word -> Primitive.Int32.int
      val toInt64: word -> Primitive.Int64.int

      (* Overflow checking, signed interp. *)
      val toInt8X: word -> Primitive.Int8.int
      val toInt16X: word -> Primitive.Int16.int
      val toInt32X: word -> Primitive.Int32.int
      val toInt64X: word -> Primitive.Int64.int   

      (* Lowbits or zero extend. *)
      val toWord8: word -> Primitive.Word8.word
      val toWord16: word -> Primitive.Word16.word
      val toWord32: word -> Primitive.Word32.word
      val toWord64: word -> Primitive.Word64.word

      (* Lowbits or sign extend. *)
      val toWord8X: word -> Primitive.Word8.word
      val toWord16X: word -> Primitive.Word16.word
      val toWord32X: word -> Primitive.Word32.word
      val toWord64X: word -> Primitive.Word64.word
   end

functor MkWord0 (W: PRIM_WORD): WORD0 =
   struct

      open W

      val detectOverflow = Primitive.Controls.detectOverflow

      val wordSizeWord = Primitive.Word32.fromInt32Unsafe wordSize
      val wordSizeMinusOneWord = Primitive.Word32.- (wordSizeWord, 0w1)

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
         if Primitive.Word32.>= (n, wordSizeWord)
            then zero
            else <<? (w, n)
      fun >> (w, n) =
         if Primitive.Word32.>= (n, wordSizeWord)
            then zero
            else >>? (w, n)
      fun ~>> (w, n) =
         if Primitive.Word32.< (n, wordSizeWord)
            then ~>>? (w, n)
            else ~>>? (w, wordSizeMinusOneWord)
      fun rol (w, n) =
         let
            val n = Primitive.Word32.remUnsafe (n, wordSizeWord)
         in
            if n = 0w0
               then w
               else rolUnsafe (w, n)
         end
      fun ror (w, n) =
         let
            val n = Primitive.Word32.remUnsafe (n, wordSizeWord)
         in
            if n = 0w0
               then w
               else rorUnsafe (w, n)
         end
      fun log2 w =
         let
            fun loop (n, s, acc) =
               if n = one
                  then acc
                  else let
                          val (n, acc) =
                            if n >= << (one, s)
                               then (>> (n, s), Primitive.Word32.+ (acc, s))
                               else (n, acc)
                       in
                          loop (n, Primitive.Word32.>>? (s, 0w1), acc)
                       end
         in
            Primitive.Word32.toInt32Unsafe
            (loop (w, Primitive.Word32.>>? (wordSizeWord, 0w1), 0w0))
         end

      local
         fun 'a make {fromIntUnsafe: 'a -> word, (* fromIntZUnsafe: 'a -> word, *)
                      toIntUnsafe: word -> 'a, toIntXUnsafe: word -> 'a,
                      other : {precision': Primitive.Int32.int,
                               maxInt': 'a,
                               minInt': 'a}} =
            let
               fun toInt w =
                  if detectOverflow
                     andalso Primitive.Int32.>= (wordSize, #precision' other)
                     andalso w > fromIntUnsafe (#maxInt' other)
                     then raise Overflow
                     else toIntUnsafe w
               fun toIntX w =
                  if detectOverflow
                     andalso Primitive.Int32.> (wordSize, #precision' other)
                     andalso fromIntUnsafe (#maxInt' other) < w
                     andalso w < fromIntUnsafe (#minInt' other)
                     then raise Overflow
                     else toIntXUnsafe w
            in
               (fromIntUnsafe,
                (* fromIntZUnsafe, *)
                toInt,
                toIntX)
            end
      in
         val (fromInt8, (* fromInt8Z, *) toInt8, toInt8X) = 
            make {fromIntUnsafe = fromInt8Unsafe,
                  (* fromIntZUnsafe = fromInt8ZUnsafe, *)
                  toIntUnsafe = toInt8Unsafe,
                  toIntXUnsafe = toInt8XUnsafe,
                  other = {precision' = Primitive.Int8.precision',
                           maxInt' = Primitive.Int8.maxInt',
                           minInt' = Primitive.Int8.minInt'}}
         val (fromInt16, (* fromInt16Z, *) toInt16, toInt16X) = 
            make {fromIntUnsafe = fromInt16Unsafe,
                  (* fromIntZUnsafe = fromInt16ZUnsafe, *)
                  toIntUnsafe = toInt16Unsafe,
                  toIntXUnsafe = toInt16XUnsafe,
                  other = {precision' = Primitive.Int16.precision',
                           maxInt' = Primitive.Int16.maxInt',
                           minInt' = Primitive.Int16.minInt'}}
         val (fromInt32, (* fromInt32Z, *) toInt32, toInt32X) = 
            make {fromIntUnsafe = fromInt32Unsafe,
                  (* fromIntZUnsafe = fromInt32ZUnsafe, *)
                  toIntUnsafe = toInt32Unsafe,
                  toIntXUnsafe = toInt32XUnsafe,
                  other = {precision' = Primitive.Int32.precision',
                           maxInt' = Primitive.Int32.maxInt',
                           minInt' = Primitive.Int32.minInt'}}
         val (fromInt64, (* fromInt64Z, *) toInt64, toInt64X) = 
            make {fromIntUnsafe = fromInt64Unsafe,
                  (* fromIntZUnsafe = fromInt64ZUnsafe, *)
                  toIntUnsafe = toInt64Unsafe,
                  toIntXUnsafe = toInt64XUnsafe,
                  other = {precision' = Primitive.Int64.precision',
                           maxInt' = Primitive.Int64.maxInt',
                           minInt' = Primitive.Int64.minInt'}}
      end

      val (fromWord8, fromWord8X, toWord8, toWord8X) =
         (fromWord8Unsafe, fromWord8XUnsafe, toWord8Unsafe, toWord8XUnsafe)   
      val (fromWord16, fromWord16X, toWord16, toWord16X) =
         (fromWord16Unsafe, fromWord16XUnsafe, toWord16Unsafe, toWord16XUnsafe)   
      val (fromWord32, fromWord32X, toWord32, toWord32X) =
         (fromWord32Unsafe, fromWord32XUnsafe, toWord32Unsafe, toWord32XUnsafe)   
      val (fromWord64, fromWord64X, toWord64, toWord64X) =
         (fromWord64Unsafe, fromWord64XUnsafe, toWord64Unsafe, toWord64XUnsafe)   

   end

structure Primitive = struct
open Primitive

structure Word8 = MkWord0 (Primitive.Word8)
structure Word16 = MkWord0 (Primitive.Word16)
structure Word32 = MkWord0 (Primitive.Word32)
structure Word64 = MkWord0 (Primitive.Word64)

end
