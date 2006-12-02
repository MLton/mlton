(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_INTWORD_CONV =
   sig
      include PRIM_INTWORD_CONV

      (* C-like cast: extend according to signedness of from or low-bits *)
      val castFromInt8ToInt8: Primitive.Int8.int -> Primitive.Int8.int
      val castFromInt8ToInt16: Primitive.Int8.int -> Primitive.Int16.int
      val castFromInt8ToInt32: Primitive.Int8.int -> Primitive.Int32.int
      val castFromInt8ToInt64: Primitive.Int8.int -> Primitive.Int64.int
      val castFromInt8ToWord8: Primitive.Int8.int -> Primitive.Word8.word
      val castFromInt8ToWord16: Primitive.Int8.int -> Primitive.Word16.word
      val castFromInt8ToWord32: Primitive.Int8.int -> Primitive.Word32.word
      val castFromInt8ToWord64: Primitive.Int8.int -> Primitive.Word64.word

      val castFromInt16ToInt8: Primitive.Int16.int -> Primitive.Int8.int
      val castFromInt16ToInt16: Primitive.Int16.int -> Primitive.Int16.int
      val castFromInt16ToInt32: Primitive.Int16.int -> Primitive.Int32.int
      val castFromInt16ToInt64: Primitive.Int16.int -> Primitive.Int64.int
      val castFromInt16ToWord8: Primitive.Int16.int -> Primitive.Word8.word
      val castFromInt16ToWord16: Primitive.Int16.int -> Primitive.Word16.word
      val castFromInt16ToWord32: Primitive.Int16.int -> Primitive.Word32.word
      val castFromInt16ToWord64: Primitive.Int16.int -> Primitive.Word64.word

      val castFromInt32ToInt8: Primitive.Int32.int -> Primitive.Int8.int
      val castFromInt32ToInt16: Primitive.Int32.int -> Primitive.Int16.int
      val castFromInt32ToInt32: Primitive.Int32.int -> Primitive.Int32.int
      val castFromInt32ToInt64: Primitive.Int32.int -> Primitive.Int64.int
      val castFromInt32ToWord8: Primitive.Int32.int -> Primitive.Word8.word
      val castFromInt32ToWord16: Primitive.Int32.int -> Primitive.Word16.word
      val castFromInt32ToWord32: Primitive.Int32.int -> Primitive.Word32.word
      val castFromInt32ToWord64: Primitive.Int32.int -> Primitive.Word64.word

      val castFromInt64ToInt8: Primitive.Int64.int -> Primitive.Int8.int
      val castFromInt64ToInt16: Primitive.Int64.int -> Primitive.Int16.int
      val castFromInt64ToInt32: Primitive.Int64.int -> Primitive.Int32.int
      val castFromInt64ToInt64: Primitive.Int64.int -> Primitive.Int64.int
      val castFromInt64ToWord8: Primitive.Int64.int -> Primitive.Word8.word
      val castFromInt64ToWord16: Primitive.Int64.int -> Primitive.Word16.word
      val castFromInt64ToWord32: Primitive.Int64.int -> Primitive.Word32.word
      val castFromInt64ToWord64: Primitive.Int64.int -> Primitive.Word64.word

      val castFromWord8ToInt8: Primitive.Word8.word -> Primitive.Int8.int
      val castFromWord8ToInt16: Primitive.Word8.word -> Primitive.Int16.int
      val castFromWord8ToInt32: Primitive.Word8.word -> Primitive.Int32.int
      val castFromWord8ToInt64: Primitive.Word8.word -> Primitive.Int64.int
      val castFromWord8ToWord8: Primitive.Word8.word -> Primitive.Word8.word
      val castFromWord8ToWord16: Primitive.Word8.word -> Primitive.Word16.word
      val castFromWord8ToWord32: Primitive.Word8.word -> Primitive.Word32.word
      val castFromWord8ToWord64: Primitive.Word8.word -> Primitive.Word64.word

      val castFromWord16ToInt8: Primitive.Word16.word -> Primitive.Int8.int
      val castFromWord16ToInt16: Primitive.Word16.word -> Primitive.Int16.int
      val castFromWord16ToInt32: Primitive.Word16.word -> Primitive.Int32.int
      val castFromWord16ToInt64: Primitive.Word16.word -> Primitive.Int64.int
      val castFromWord16ToWord8: Primitive.Word16.word -> Primitive.Word8.word
      val castFromWord16ToWord16: Primitive.Word16.word -> Primitive.Word16.word
      val castFromWord16ToWord32: Primitive.Word16.word -> Primitive.Word32.word
      val castFromWord16ToWord64: Primitive.Word16.word -> Primitive.Word64.word

      val castFromWord32ToInt8: Primitive.Word32.word -> Primitive.Int8.int
      val castFromWord32ToInt16: Primitive.Word32.word -> Primitive.Int16.int
      val castFromWord32ToInt32: Primitive.Word32.word -> Primitive.Int32.int
      val castFromWord32ToInt64: Primitive.Word32.word -> Primitive.Int64.int
      val castFromWord32ToWord8: Primitive.Word32.word -> Primitive.Word8.word
      val castFromWord32ToWord16: Primitive.Word32.word -> Primitive.Word16.word
      val castFromWord32ToWord32: Primitive.Word32.word -> Primitive.Word32.word
      val castFromWord32ToWord64: Primitive.Word32.word -> Primitive.Word64.word

      val castFromWord64ToInt8: Primitive.Word64.word -> Primitive.Int8.int
      val castFromWord64ToInt16: Primitive.Word64.word -> Primitive.Int16.int
      val castFromWord64ToInt32: Primitive.Word64.word -> Primitive.Int32.int
      val castFromWord64ToInt64: Primitive.Word64.word -> Primitive.Int64.int
      val castFromWord64ToWord8: Primitive.Word64.word -> Primitive.Word8.word
      val castFromWord64ToWord16: Primitive.Word64.word -> Primitive.Word16.word
      val castFromWord64ToWord32: Primitive.Word64.word -> Primitive.Word32.word
      val castFromWord64ToWord64: Primitive.Word64.word -> Primitive.Word64.word

      (* checked zero-extend or low-bits, 
       * Overflow if composed zero-extend not identity
       *)
      val zchckFromInt8ToInt8: Primitive.Int8.int -> Primitive.Int8.int
      val zchckFromInt8ToInt16: Primitive.Int8.int -> Primitive.Int16.int
      val zchckFromInt8ToInt32: Primitive.Int8.int -> Primitive.Int32.int
      val zchckFromInt8ToInt64: Primitive.Int8.int -> Primitive.Int64.int
      val zchckFromInt8ToWord8: Primitive.Int8.int -> Primitive.Word8.word
      val zchckFromInt8ToWord16: Primitive.Int8.int -> Primitive.Word16.word
      val zchckFromInt8ToWord32: Primitive.Int8.int -> Primitive.Word32.word
      val zchckFromInt8ToWord64: Primitive.Int8.int -> Primitive.Word64.word

      val zchckFromInt16ToInt8: Primitive.Int16.int -> Primitive.Int8.int
      val zchckFromInt16ToInt16: Primitive.Int16.int -> Primitive.Int16.int
      val zchckFromInt16ToInt32: Primitive.Int16.int -> Primitive.Int32.int
      val zchckFromInt16ToInt64: Primitive.Int16.int -> Primitive.Int64.int
      val zchckFromInt16ToWord8: Primitive.Int16.int -> Primitive.Word8.word
      val zchckFromInt16ToWord16: Primitive.Int16.int -> Primitive.Word16.word
      val zchckFromInt16ToWord32: Primitive.Int16.int -> Primitive.Word32.word
      val zchckFromInt16ToWord64: Primitive.Int16.int -> Primitive.Word64.word

      val zchckFromInt32ToInt8: Primitive.Int32.int -> Primitive.Int8.int
      val zchckFromInt32ToInt16: Primitive.Int32.int -> Primitive.Int16.int
      val zchckFromInt32ToInt32: Primitive.Int32.int -> Primitive.Int32.int
      val zchckFromInt32ToInt64: Primitive.Int32.int -> Primitive.Int64.int
      val zchckFromInt32ToWord8: Primitive.Int32.int -> Primitive.Word8.word
      val zchckFromInt32ToWord16: Primitive.Int32.int -> Primitive.Word16.word
      val zchckFromInt32ToWord32: Primitive.Int32.int -> Primitive.Word32.word
      val zchckFromInt32ToWord64: Primitive.Int32.int -> Primitive.Word64.word

      val zchckFromInt64ToInt8: Primitive.Int64.int -> Primitive.Int8.int
      val zchckFromInt64ToInt16: Primitive.Int64.int -> Primitive.Int16.int
      val zchckFromInt64ToInt32: Primitive.Int64.int -> Primitive.Int32.int
      val zchckFromInt64ToInt64: Primitive.Int64.int -> Primitive.Int64.int
      val zchckFromInt64ToWord8: Primitive.Int64.int -> Primitive.Word8.word
      val zchckFromInt64ToWord16: Primitive.Int64.int -> Primitive.Word16.word
      val zchckFromInt64ToWord32: Primitive.Int64.int -> Primitive.Word32.word
      val zchckFromInt64ToWord64: Primitive.Int64.int -> Primitive.Word64.word

      val zchckFromWord8ToInt8: Primitive.Word8.word -> Primitive.Int8.int
      val zchckFromWord8ToInt16: Primitive.Word8.word -> Primitive.Int16.int
      val zchckFromWord8ToInt32: Primitive.Word8.word -> Primitive.Int32.int
      val zchckFromWord8ToInt64: Primitive.Word8.word -> Primitive.Int64.int
      val zchckFromWord8ToWord8: Primitive.Word8.word -> Primitive.Word8.word
      val zchckFromWord8ToWord16: Primitive.Word8.word -> Primitive.Word16.word
      val zchckFromWord8ToWord32: Primitive.Word8.word -> Primitive.Word32.word
      val zchckFromWord8ToWord64: Primitive.Word8.word -> Primitive.Word64.word

      val zchckFromWord16ToInt8: Primitive.Word16.word -> Primitive.Int8.int
      val zchckFromWord16ToInt16: Primitive.Word16.word -> Primitive.Int16.int
      val zchckFromWord16ToInt32: Primitive.Word16.word -> Primitive.Int32.int
      val zchckFromWord16ToInt64: Primitive.Word16.word -> Primitive.Int64.int
      val zchckFromWord16ToWord8: Primitive.Word16.word -> Primitive.Word8.word
      val zchckFromWord16ToWord16: Primitive.Word16.word -> Primitive.Word16.word
      val zchckFromWord16ToWord32: Primitive.Word16.word -> Primitive.Word32.word
      val zchckFromWord16ToWord64: Primitive.Word16.word -> Primitive.Word64.word

      val zchckFromWord32ToInt8: Primitive.Word32.word -> Primitive.Int8.int
      val zchckFromWord32ToInt16: Primitive.Word32.word -> Primitive.Int16.int
      val zchckFromWord32ToInt32: Primitive.Word32.word -> Primitive.Int32.int
      val zchckFromWord32ToInt64: Primitive.Word32.word -> Primitive.Int64.int
      val zchckFromWord32ToWord8: Primitive.Word32.word -> Primitive.Word8.word
      val zchckFromWord32ToWord16: Primitive.Word32.word -> Primitive.Word16.word
      val zchckFromWord32ToWord32: Primitive.Word32.word -> Primitive.Word32.word
      val zchckFromWord32ToWord64: Primitive.Word32.word -> Primitive.Word64.word

      val zchckFromWord64ToInt8: Primitive.Word64.word -> Primitive.Int8.int
      val zchckFromWord64ToInt16: Primitive.Word64.word -> Primitive.Int16.int
      val zchckFromWord64ToInt32: Primitive.Word64.word -> Primitive.Int32.int
      val zchckFromWord64ToInt64: Primitive.Word64.word -> Primitive.Int64.int
      val zchckFromWord64ToWord8: Primitive.Word64.word -> Primitive.Word8.word
      val zchckFromWord64ToWord16: Primitive.Word64.word -> Primitive.Word16.word
      val zchckFromWord64ToWord32: Primitive.Word64.word -> Primitive.Word32.word
      val zchckFromWord64ToWord64: Primitive.Word64.word -> Primitive.Word64.word

      (* checked sign-extend or low-bits, 
       * Overflow if composed sign-extend not identity
       *)
      val schckFromInt8ToInt8: Primitive.Int8.int -> Primitive.Int8.int
      val schckFromInt8ToInt16: Primitive.Int8.int -> Primitive.Int16.int
      val schckFromInt8ToInt32: Primitive.Int8.int -> Primitive.Int32.int
      val schckFromInt8ToInt64: Primitive.Int8.int -> Primitive.Int64.int
      val schckFromInt8ToWord8: Primitive.Int8.int -> Primitive.Word8.word
      val schckFromInt8ToWord16: Primitive.Int8.int -> Primitive.Word16.word
      val schckFromInt8ToWord32: Primitive.Int8.int -> Primitive.Word32.word
      val schckFromInt8ToWord64: Primitive.Int8.int -> Primitive.Word64.word

      val schckFromInt16ToInt8: Primitive.Int16.int -> Primitive.Int8.int
      val schckFromInt16ToInt16: Primitive.Int16.int -> Primitive.Int16.int
      val schckFromInt16ToInt32: Primitive.Int16.int -> Primitive.Int32.int
      val schckFromInt16ToInt64: Primitive.Int16.int -> Primitive.Int64.int
      val schckFromInt16ToWord8: Primitive.Int16.int -> Primitive.Word8.word
      val schckFromInt16ToWord16: Primitive.Int16.int -> Primitive.Word16.word
      val schckFromInt16ToWord32: Primitive.Int16.int -> Primitive.Word32.word
      val schckFromInt16ToWord64: Primitive.Int16.int -> Primitive.Word64.word

      val schckFromInt32ToInt8: Primitive.Int32.int -> Primitive.Int8.int
      val schckFromInt32ToInt16: Primitive.Int32.int -> Primitive.Int16.int
      val schckFromInt32ToInt32: Primitive.Int32.int -> Primitive.Int32.int
      val schckFromInt32ToInt64: Primitive.Int32.int -> Primitive.Int64.int
      val schckFromInt32ToWord8: Primitive.Int32.int -> Primitive.Word8.word
      val schckFromInt32ToWord16: Primitive.Int32.int -> Primitive.Word16.word
      val schckFromInt32ToWord32: Primitive.Int32.int -> Primitive.Word32.word
      val schckFromInt32ToWord64: Primitive.Int32.int -> Primitive.Word64.word

      val schckFromInt64ToInt8: Primitive.Int64.int -> Primitive.Int8.int
      val schckFromInt64ToInt16: Primitive.Int64.int -> Primitive.Int16.int
      val schckFromInt64ToInt32: Primitive.Int64.int -> Primitive.Int32.int
      val schckFromInt64ToInt64: Primitive.Int64.int -> Primitive.Int64.int
      val schckFromInt64ToWord8: Primitive.Int64.int -> Primitive.Word8.word
      val schckFromInt64ToWord16: Primitive.Int64.int -> Primitive.Word16.word
      val schckFromInt64ToWord32: Primitive.Int64.int -> Primitive.Word32.word
      val schckFromInt64ToWord64: Primitive.Int64.int -> Primitive.Word64.word

      val schckFromWord8ToInt8: Primitive.Word8.word -> Primitive.Int8.int
      val schckFromWord8ToInt16: Primitive.Word8.word -> Primitive.Int16.int
      val schckFromWord8ToInt32: Primitive.Word8.word -> Primitive.Int32.int
      val schckFromWord8ToInt64: Primitive.Word8.word -> Primitive.Int64.int
      val schckFromWord8ToWord8: Primitive.Word8.word -> Primitive.Word8.word
      val schckFromWord8ToWord16: Primitive.Word8.word -> Primitive.Word16.word
      val schckFromWord8ToWord32: Primitive.Word8.word -> Primitive.Word32.word
      val schckFromWord8ToWord64: Primitive.Word8.word -> Primitive.Word64.word

      val schckFromWord16ToInt8: Primitive.Word16.word -> Primitive.Int8.int
      val schckFromWord16ToInt16: Primitive.Word16.word -> Primitive.Int16.int
      val schckFromWord16ToInt32: Primitive.Word16.word -> Primitive.Int32.int
      val schckFromWord16ToInt64: Primitive.Word16.word -> Primitive.Int64.int
      val schckFromWord16ToWord8: Primitive.Word16.word -> Primitive.Word8.word
      val schckFromWord16ToWord16: Primitive.Word16.word -> Primitive.Word16.word
      val schckFromWord16ToWord32: Primitive.Word16.word -> Primitive.Word32.word
      val schckFromWord16ToWord64: Primitive.Word16.word -> Primitive.Word64.word

      val schckFromWord32ToInt8: Primitive.Word32.word -> Primitive.Int8.int
      val schckFromWord32ToInt16: Primitive.Word32.word -> Primitive.Int16.int
      val schckFromWord32ToInt32: Primitive.Word32.word -> Primitive.Int32.int
      val schckFromWord32ToInt64: Primitive.Word32.word -> Primitive.Int64.int
      val schckFromWord32ToWord8: Primitive.Word32.word -> Primitive.Word8.word
      val schckFromWord32ToWord16: Primitive.Word32.word -> Primitive.Word16.word
      val schckFromWord32ToWord32: Primitive.Word32.word -> Primitive.Word32.word
      val schckFromWord32ToWord64: Primitive.Word32.word -> Primitive.Word64.word

      val schckFromWord64ToInt8: Primitive.Word64.word -> Primitive.Int8.int
      val schckFromWord64ToInt16: Primitive.Word64.word -> Primitive.Int16.int
      val schckFromWord64ToInt32: Primitive.Word64.word -> Primitive.Int32.int
      val schckFromWord64ToInt64: Primitive.Word64.word -> Primitive.Int64.int
      val schckFromWord64ToWord8: Primitive.Word64.word -> Primitive.Word8.word
      val schckFromWord64ToWord16: Primitive.Word64.word -> Primitive.Word16.word
      val schckFromWord64ToWord32: Primitive.Word64.word -> Primitive.Word32.word
      val schckFromWord64ToWord64: Primitive.Word64.word -> Primitive.Word64.word
   end
signature PRIM_INTEGER =
   sig
      include PRIM_INTEGER

      val zextdFromInt8: Primitive.Int8.int -> int
      val zextdFromInt16: Primitive.Int16.int -> int
      val zextdFromInt32: Primitive.Int32.int -> int
      val zextdFromInt64: Primitive.Int64.int -> int
      val zextdFromWord8: Primitive.Word8.word -> int
      val zextdFromWord16: Primitive.Word16.word -> int
      val zextdFromWord32: Primitive.Word32.word -> int
      val zextdFromWord64: Primitive.Word64.word -> int
      val zextdToInt8: int -> Primitive.Int8.int
      val zextdToInt16: int -> Primitive.Int16.int
      val zextdToInt32: int -> Primitive.Int32.int
      val zextdToInt64: int -> Primitive.Int64.int
      val zextdToWord8: int -> Primitive.Word8.word
      val zextdToWord16: int -> Primitive.Word16.word
      val zextdToWord32: int -> Primitive.Word32.word
      val zextdToWord64: int -> Primitive.Word64.word

      val sextdFromInt8: Primitive.Int8.int -> int
      val sextdFromInt16: Primitive.Int16.int -> int
      val sextdFromInt32: Primitive.Int32.int -> int
      val sextdFromInt64: Primitive.Int64.int -> int
      val sextdFromWord8: Primitive.Word8.word -> int
      val sextdFromWord16: Primitive.Word16.word -> int
      val sextdFromWord32: Primitive.Word32.word -> int
      val sextdFromWord64: Primitive.Word64.word -> int
      val sextdToInt8: int -> Primitive.Int8.int
      val sextdToInt16: int -> Primitive.Int16.int
      val sextdToInt32: int -> Primitive.Int32.int
      val sextdToInt64: int -> Primitive.Int64.int
      val sextdToWord8: int -> Primitive.Word8.word
      val sextdToWord16: int -> Primitive.Word16.word
      val sextdToWord32: int -> Primitive.Word32.word
      val sextdToWord64: int -> Primitive.Word64.word

      val castFromInt8: Primitive.Int8.int -> int
      val castFromInt16: Primitive.Int16.int -> int
      val castFromInt32: Primitive.Int32.int -> int
      val castFromInt64: Primitive.Int64.int -> int
      val castFromWord8: Primitive.Word8.word -> int
      val castFromWord16: Primitive.Word16.word -> int
      val castFromWord32: Primitive.Word32.word -> int
      val castFromWord64: Primitive.Word64.word -> int
      val castToInt8: int -> Primitive.Int8.int
      val castToInt16: int -> Primitive.Int16.int
      val castToInt32: int -> Primitive.Int32.int
      val castToInt64: int -> Primitive.Int64.int
      val castToWord8: int -> Primitive.Word8.word
      val castToWord16: int -> Primitive.Word16.word
      val castToWord32: int -> Primitive.Word32.word
      val castToWord64: int -> Primitive.Word64.word

      val zchckFromInt8: Primitive.Int8.int -> int
      val zchckFromInt16: Primitive.Int16.int -> int
      val zchckFromInt32: Primitive.Int32.int -> int
      val zchckFromInt64: Primitive.Int64.int -> int
      val zchckFromWord8: Primitive.Word8.word -> int
      val zchckFromWord16: Primitive.Word16.word -> int
      val zchckFromWord32: Primitive.Word32.word -> int
      val zchckFromWord64: Primitive.Word64.word -> int
      val zchckToInt8: int -> Primitive.Int8.int
      val zchckToInt16: int -> Primitive.Int16.int
      val zchckToInt32: int -> Primitive.Int32.int
      val zchckToInt64: int -> Primitive.Int64.int
      val zchckToWord8: int -> Primitive.Word8.word
      val zchckToWord16: int -> Primitive.Word16.word
      val zchckToWord32: int -> Primitive.Word32.word
      val zchckToWord64: int -> Primitive.Word64.word

      val schckFromInt8: Primitive.Int8.int -> int
      val schckFromInt16: Primitive.Int16.int -> int
      val schckFromInt32: Primitive.Int32.int -> int
      val schckFromInt64: Primitive.Int64.int -> int
      val schckFromWord8: Primitive.Word8.word -> int
      val schckFromWord16: Primitive.Word16.word -> int
      val schckFromWord32: Primitive.Word32.word -> int
      val schckFromWord64: Primitive.Word64.word -> int
      val schckToInt8: int -> Primitive.Int8.int
      val schckToInt16: int -> Primitive.Int16.int
      val schckToInt32: int -> Primitive.Int32.int
      val schckToInt64: int -> Primitive.Int64.int
      val schckToWord8: int -> Primitive.Word8.word
      val schckToWord16: int -> Primitive.Word16.word
      val schckToWord32: int -> Primitive.Word32.word
      val schckToWord64: int -> Primitive.Word64.word
   end
signature PRIM_WORD =
   sig
      include PRIM_WORD

      val zextdFromInt8: Primitive.Int8.int -> word
      val zextdFromInt16: Primitive.Int16.int -> word
      val zextdFromInt32: Primitive.Int32.int -> word
      val zextdFromInt64: Primitive.Int64.int -> word
      val zextdFromWord8: Primitive.Word8.word -> word
      val zextdFromWord16: Primitive.Word16.word -> word
      val zextdFromWord32: Primitive.Word32.word -> word
      val zextdFromWord64: Primitive.Word64.word -> word
      val zextdToInt8: word -> Primitive.Int8.int
      val zextdToInt16: word -> Primitive.Int16.int
      val zextdToInt32: word -> Primitive.Int32.int
      val zextdToInt64: word -> Primitive.Int64.int
      val zextdToWord8: word -> Primitive.Word8.word
      val zextdToWord16: word -> Primitive.Word16.word
      val zextdToWord32: word -> Primitive.Word32.word
      val zextdToWord64: word -> Primitive.Word64.word

      val sextdFromInt8: Primitive.Int8.int -> word
      val sextdFromInt16: Primitive.Int16.int -> word
      val sextdFromInt32: Primitive.Int32.int -> word
      val sextdFromInt64: Primitive.Int64.int -> word
      val sextdFromWord8: Primitive.Word8.word -> word
      val sextdFromWord16: Primitive.Word16.word -> word
      val sextdFromWord32: Primitive.Word32.word -> word
      val sextdFromWord64: Primitive.Word64.word -> word
      val sextdToInt8: word -> Primitive.Int8.int
      val sextdToInt16: word -> Primitive.Int16.int
      val sextdToInt32: word -> Primitive.Int32.int
      val sextdToInt64: word -> Primitive.Int64.int
      val sextdToWord8: word -> Primitive.Word8.word
      val sextdToWord16: word -> Primitive.Word16.word
      val sextdToWord32: word -> Primitive.Word32.word
      val sextdToWord64: word -> Primitive.Word64.word

      val castFromInt8: Primitive.Int8.int -> word
      val castFromInt16: Primitive.Int16.int -> word
      val castFromInt32: Primitive.Int32.int -> word
      val castFromInt64: Primitive.Int64.int -> word
      val castFromWord8: Primitive.Word8.word -> word
      val castFromWord16: Primitive.Word16.word -> word
      val castFromWord32: Primitive.Word32.word -> word
      val castFromWord64: Primitive.Word64.word -> word
      val castToInt8: word -> Primitive.Int8.int
      val castToInt16: word -> Primitive.Int16.int
      val castToInt32: word -> Primitive.Int32.int
      val castToInt64: word -> Primitive.Int64.int
      val castToWord8: word -> Primitive.Word8.word
      val castToWord16: word -> Primitive.Word16.word
      val castToWord32: word -> Primitive.Word32.word
      val castToWord64: word -> Primitive.Word64.word

      val zchckFromInt8: Primitive.Int8.int -> word
      val zchckFromInt16: Primitive.Int16.int -> word
      val zchckFromInt32: Primitive.Int32.int -> word
      val zchckFromInt64: Primitive.Int64.int -> word
      val zchckFromWord8: Primitive.Word8.word -> word
      val zchckFromWord16: Primitive.Word16.word -> word
      val zchckFromWord32: Primitive.Word32.word -> word
      val zchckFromWord64: Primitive.Word64.word -> word
      val zchckToInt8: word -> Primitive.Int8.int
      val zchckToInt16: word -> Primitive.Int16.int
      val zchckToInt32: word -> Primitive.Int32.int
      val zchckToInt64: word -> Primitive.Int64.int
      val zchckToWord8: word -> Primitive.Word8.word
      val zchckToWord16: word -> Primitive.Word16.word
      val zchckToWord32: word -> Primitive.Word32.word
      val zchckToWord64: word -> Primitive.Word64.word

      val schckFromInt8: Primitive.Int8.int -> word
      val schckFromInt16: Primitive.Int16.int -> word
      val schckFromInt32: Primitive.Int32.int -> word
      val schckFromInt64: Primitive.Int64.int -> word
      val schckFromWord8: Primitive.Word8.word -> word
      val schckFromWord16: Primitive.Word16.word -> word
      val schckFromWord32: Primitive.Word32.word -> word
      val schckFromWord64: Primitive.Word64.word -> word
      val schckToInt8: word -> Primitive.Int8.int
      val schckToInt16: word -> Primitive.Int16.int
      val schckToInt32: word -> Primitive.Int32.int
      val schckToInt64: word -> Primitive.Int64.int
      val schckToWord8: word -> Primitive.Word8.word
      val schckToWord16: word -> Primitive.Word16.word
      val schckToWord32: word -> Primitive.Word32.word
      val schckToWord64: word -> Primitive.Word64.word
   end

structure Primitive = struct

open Primitive

structure IntWordConv : PRIM_INTWORD_CONV =
   struct
      open IntWordConv

      (* C-like cast: extend according to signedness of from or low-bits *)
      val castFromInt8ToInt8 = sextdFromInt8ToInt8
      val castFromInt8ToInt16 = sextdFromInt8ToInt16
      val castFromInt8ToInt32 = sextdFromInt8ToInt32
      val castFromInt8ToInt64 = sextdFromInt8ToInt64
      val castFromInt8ToWord8 = sextdFromInt8ToWord8
      val castFromInt8ToWord16 = sextdFromInt8ToWord16
      val castFromInt8ToWord32 = sextdFromInt8ToWord32
      val castFromInt8ToWord64 = sextdFromInt8ToWord64
      val castFromInt16ToInt8 = sextdFromInt16ToInt8
      val castFromInt16ToInt16 = sextdFromInt16ToInt16
      val castFromInt16ToInt32 = sextdFromInt16ToInt32
      val castFromInt16ToInt64 = sextdFromInt16ToInt64
      val castFromInt16ToWord8 = sextdFromInt16ToWord8
      val castFromInt16ToWord16 = sextdFromInt16ToWord16
      val castFromInt16ToWord32 = sextdFromInt16ToWord32
      val castFromInt16ToWord64 = sextdFromInt16ToWord64
      val castFromInt32ToInt8 = sextdFromInt32ToInt8
      val castFromInt32ToInt16 = sextdFromInt32ToInt16
      val castFromInt32ToInt32 = sextdFromInt32ToInt32
      val castFromInt32ToInt64 = sextdFromInt32ToInt64
      val castFromInt32ToWord8 = sextdFromInt32ToWord8
      val castFromInt32ToWord16 = sextdFromInt32ToWord16
      val castFromInt32ToWord32 = sextdFromInt32ToWord32
      val castFromInt32ToWord64 = sextdFromInt32ToWord64
      val castFromInt64ToInt8 = sextdFromInt64ToInt8
      val castFromInt64ToInt16 = sextdFromInt64ToInt16
      val castFromInt64ToInt32 = sextdFromInt64ToInt32
      val castFromInt64ToInt64 = sextdFromInt64ToInt64
      val castFromInt64ToWord8 = sextdFromInt64ToWord8
      val castFromInt64ToWord16 = sextdFromInt64ToWord16
      val castFromInt64ToWord32 = sextdFromInt64ToWord32
      val castFromInt64ToWord64 = sextdFromInt64ToWord64

      val castFromWord8ToInt8 = zextdFromWord8ToInt8
      val castFromWord8ToInt16 = zextdFromWord8ToInt16
      val castFromWord8ToInt32 = zextdFromWord8ToInt32
      val castFromWord8ToInt64 = zextdFromWord8ToInt64
      val castFromWord8ToWord8 = zextdFromWord8ToWord8
      val castFromWord8ToWord16 = zextdFromWord8ToWord16
      val castFromWord8ToWord32 = zextdFromWord8ToWord32
      val castFromWord8ToWord64 = zextdFromWord8ToWord64
      val castFromWord16ToInt8 = zextdFromWord16ToInt8
      val castFromWord16ToInt16 = zextdFromWord16ToInt16
      val castFromWord16ToInt32 = zextdFromWord16ToInt32
      val castFromWord16ToInt64 = zextdFromWord16ToInt64
      val castFromWord16ToWord8 = zextdFromWord16ToWord8
      val castFromWord16ToWord16 = zextdFromWord16ToWord16
      val castFromWord16ToWord32 = zextdFromWord16ToWord32
      val castFromWord16ToWord64 = zextdFromWord16ToWord64
      val castFromWord32ToInt8 = zextdFromWord32ToInt8
      val castFromWord32ToInt16 = zextdFromWord32ToInt16
      val castFromWord32ToInt32 = zextdFromWord32ToInt32
      val castFromWord32ToInt64 = zextdFromWord32ToInt64
      val castFromWord32ToWord8 = zextdFromWord32ToWord8
      val castFromWord32ToWord16 = zextdFromWord32ToWord16
      val castFromWord32ToWord32 = zextdFromWord32ToWord32
      val castFromWord32ToWord64 = zextdFromWord32ToWord64
      val castFromWord64ToInt8 = zextdFromWord64ToInt8
      val castFromWord64ToInt16 = zextdFromWord64ToInt16
      val castFromWord64ToInt32 = zextdFromWord64ToInt32
      val castFromWord64ToInt64 = zextdFromWord64ToInt64
      val castFromWord64ToWord8 = zextdFromWord64ToWord8
      val castFromWord64ToWord16 = zextdFromWord64ToWord16
      val castFromWord64ToWord32 = zextdFromWord64ToWord32
      val castFromWord64ToWord64 = zextdFromWord64ToWord64

      (* checked zero-extend or low-bits, 
       * Overflow if composed zero-extend not identity
       *)
      local
         fun (''l, ''s) make {zextdFromLargeToSmall: ''l -> ''s,
                              zextdFromSmallToLarge: ''s -> ''l} =
            if Primitive.Controls.detectOverflow
               then fn (x: ''l) => let 
                                      val res = zextdFromLargeToSmall x
                                   in
                                      if x = (zextdFromSmallToLarge res)
                                         then res
                                         else raise Overflow
                                   end
               else zextdFromLargeToSmall
      in
         val zchckFromInt8ToInt8 = zextdFromInt8ToInt8
         val zchckFromInt8ToInt16 = zextdFromInt8ToInt16
         val zchckFromInt8ToInt32 = zextdFromInt8ToInt32
         val zchckFromInt8ToInt64 = zextdFromInt8ToInt64
         val zchckFromInt8ToWord8 = zextdFromInt8ToWord8
         val zchckFromInt8ToWord16 = zextdFromInt8ToWord16
         val zchckFromInt8ToWord32 = zextdFromInt8ToWord32
         val zchckFromInt8ToWord64 = zextdFromInt8ToWord64
         val zchckFromInt16ToInt8 = 
            make {zextdFromLargeToSmall = zextdFromInt16ToInt8,
                  zextdFromSmallToLarge = zextdFromInt8ToInt16}
         val zchckFromInt16ToInt16 = zextdFromInt16ToInt16
         val zchckFromInt16ToInt32 = zextdFromInt16ToInt32
         val zchckFromInt16ToInt64 = zextdFromInt16ToInt64
         val zchckFromInt16ToWord8 = 
            make {zextdFromLargeToSmall = zextdFromInt16ToWord8,
                  zextdFromSmallToLarge = zextdFromWord8ToInt16}
         val zchckFromInt16ToWord16 = zextdFromInt16ToWord16
         val zchckFromInt16ToWord32 = zextdFromInt16ToWord32
         val zchckFromInt16ToWord64 = zextdFromInt16ToWord64
         val zchckFromInt32ToInt8 = 
            make {zextdFromLargeToSmall = zextdFromInt32ToInt8,
                  zextdFromSmallToLarge = zextdFromInt8ToInt32}
         val zchckFromInt32ToInt16 = 
            make {zextdFromLargeToSmall = zextdFromInt32ToInt16,
                  zextdFromSmallToLarge = zextdFromInt16ToInt32}
         val zchckFromInt32ToInt32 = zextdFromInt32ToInt32
         val zchckFromInt32ToInt64 = zextdFromInt32ToInt64
         val zchckFromInt32ToWord8 = 
            make {zextdFromLargeToSmall = zextdFromInt32ToWord8,
                  zextdFromSmallToLarge = zextdFromWord8ToInt32}
         val zchckFromInt32ToWord16 = 
            make {zextdFromLargeToSmall = zextdFromInt32ToWord16,
                  zextdFromSmallToLarge = zextdFromWord16ToInt32}
         val zchckFromInt32ToWord32 = zextdFromInt32ToWord32
         val zchckFromInt32ToWord64 = zextdFromInt32ToWord64
         val zchckFromInt64ToInt8 = 
            make {zextdFromLargeToSmall = zextdFromInt64ToInt8,
                  zextdFromSmallToLarge = zextdFromInt8ToInt64}
         val zchckFromInt64ToInt16 = 
            make {zextdFromLargeToSmall = zextdFromInt64ToInt16,
                  zextdFromSmallToLarge = zextdFromInt16ToInt64}
         val zchckFromInt64ToInt32 = 
            make {zextdFromLargeToSmall = zextdFromInt64ToInt32,
                  zextdFromSmallToLarge = zextdFromInt32ToInt64}
         val zchckFromInt64ToInt64 = zextdFromInt64ToInt64
         val zchckFromInt64ToWord8 = 
            make {zextdFromLargeToSmall = zextdFromInt64ToWord8,
                  zextdFromSmallToLarge = zextdFromWord8ToInt64}
         val zchckFromInt64ToWord16 = 
            make {zextdFromLargeToSmall = zextdFromInt64ToWord16,
                  zextdFromSmallToLarge = zextdFromWord16ToInt64}
         val zchckFromInt64ToWord32 = 
            make {zextdFromLargeToSmall = zextdFromInt64ToWord32,
                  zextdFromSmallToLarge = zextdFromWord32ToInt64}
         val zchckFromInt64ToWord64 = zextdFromInt64ToWord64

         val zchckFromWord8ToInt8 = zextdFromWord8ToInt8
         val zchckFromWord8ToInt16 = zextdFromWord8ToInt16
         val zchckFromWord8ToInt32 = zextdFromWord8ToInt32
         val zchckFromWord8ToInt64 = zextdFromWord8ToInt64
         val zchckFromWord8ToWord8 = zextdFromWord8ToWord8
         val zchckFromWord8ToWord16 = zextdFromWord8ToWord16
         val zchckFromWord8ToWord32 = zextdFromWord8ToWord32
         val zchckFromWord8ToWord64 = zextdFromWord8ToWord64
         val zchckFromWord16ToInt8 = 
            make {zextdFromLargeToSmall = zextdFromWord16ToInt8,
                  zextdFromSmallToLarge = zextdFromInt8ToWord16}
         val zchckFromWord16ToInt16 = zextdFromWord16ToInt16
         val zchckFromWord16ToInt32 = zextdFromWord16ToInt32
         val zchckFromWord16ToInt64 = zextdFromWord16ToInt64
         val zchckFromWord16ToWord8 = 
            make {zextdFromLargeToSmall = zextdFromWord16ToWord8,
                  zextdFromSmallToLarge = zextdFromWord8ToWord16}
         val zchckFromWord16ToWord16 = zextdFromWord16ToWord16
         val zchckFromWord16ToWord32 = zextdFromWord16ToWord32
         val zchckFromWord16ToWord64 = zextdFromWord16ToWord64
         val zchckFromWord32ToInt8 = 
            make {zextdFromLargeToSmall = zextdFromWord32ToInt8,
                  zextdFromSmallToLarge = zextdFromInt8ToWord32}
         val zchckFromWord32ToInt16 = 
            make {zextdFromLargeToSmall = zextdFromWord32ToInt16,
                  zextdFromSmallToLarge = zextdFromInt16ToWord32}
         val zchckFromWord32ToInt32 = zextdFromWord32ToInt32
         val zchckFromWord32ToInt64 = zextdFromWord32ToInt64
         val zchckFromWord32ToWord8 = 
            make {zextdFromLargeToSmall = zextdFromWord32ToWord8,
                  zextdFromSmallToLarge = zextdFromWord8ToWord32}
         val zchckFromWord32ToWord16 = 
            make {zextdFromLargeToSmall = zextdFromWord32ToWord16,
                  zextdFromSmallToLarge = zextdFromWord16ToWord32}
         val zchckFromWord32ToWord32 = zextdFromWord32ToWord32
         val zchckFromWord32ToWord64 = zextdFromWord32ToWord64
         val zchckFromWord64ToInt8 = 
            make {zextdFromLargeToSmall = zextdFromWord64ToInt8,
                  zextdFromSmallToLarge = zextdFromInt8ToWord64}
         val zchckFromWord64ToInt16 = 
            make {zextdFromLargeToSmall = zextdFromWord64ToInt16,
                  zextdFromSmallToLarge = zextdFromInt16ToWord64}
         val zchckFromWord64ToInt32 = 
            make {zextdFromLargeToSmall = zextdFromWord64ToInt32,
                  zextdFromSmallToLarge = zextdFromInt32ToWord64}
         val zchckFromWord64ToInt64 = zextdFromWord64ToInt64
         val zchckFromWord64ToWord8 = 
            make {zextdFromLargeToSmall = zextdFromWord64ToWord8,
                  zextdFromSmallToLarge = zextdFromWord8ToWord64}
         val zchckFromWord64ToWord16 = 
            make {zextdFromLargeToSmall = zextdFromWord64ToWord16,
                  zextdFromSmallToLarge = zextdFromWord16ToWord64}
         val zchckFromWord64ToWord32 = 
            make {zextdFromLargeToSmall = zextdFromWord64ToWord32,
                  zextdFromSmallToLarge = zextdFromWord32ToWord64}
         val zchckFromWord64ToWord64 = zextdFromWord64ToWord64
      end

      (* checked sign-extend or low-bits, 
       * Overflow if composed sign-extend not identity
       *)
      local
         fun (''l, ''s) make {sextdFromLargeToSmall: ''l -> ''s,
                              sextdFromSmallToLarge: ''s -> ''l} =
            if Primitive.Controls.detectOverflow
               then fn (x: ''l) => let 
                                      val res = sextdFromLargeToSmall x
                                   in
                                      if x = (sextdFromSmallToLarge res)
                                         then res
                                         else raise Overflow
                                   end
               else sextdFromLargeToSmall
      in
         val schckFromInt8ToInt8 = sextdFromInt8ToInt8
         val schckFromInt8ToInt16 = sextdFromInt8ToInt16
         val schckFromInt8ToInt32 = sextdFromInt8ToInt32
         val schckFromInt8ToInt64 = sextdFromInt8ToInt64
         val schckFromInt8ToWord8 = sextdFromInt8ToWord8
         val schckFromInt8ToWord16 = sextdFromInt8ToWord16
         val schckFromInt8ToWord32 = sextdFromInt8ToWord32
         val schckFromInt8ToWord64 = sextdFromInt8ToWord64
         val schckFromInt16ToInt8 = 
            make {sextdFromLargeToSmall = sextdFromInt16ToInt8,
                  sextdFromSmallToLarge = sextdFromInt8ToInt16}
         val schckFromInt16ToInt16 = sextdFromInt16ToInt16
         val schckFromInt16ToInt32 = sextdFromInt16ToInt32
         val schckFromInt16ToInt64 = sextdFromInt16ToInt64
         val schckFromInt16ToWord8 = 
            make {sextdFromLargeToSmall = sextdFromInt16ToWord8,
                  sextdFromSmallToLarge = sextdFromWord8ToInt16}
         val schckFromInt16ToWord16 = sextdFromInt16ToWord16
         val schckFromInt16ToWord32 = sextdFromInt16ToWord32
         val schckFromInt16ToWord64 = sextdFromInt16ToWord64
         val schckFromInt32ToInt8 = 
            make {sextdFromLargeToSmall = sextdFromInt32ToInt8,
                  sextdFromSmallToLarge = sextdFromInt8ToInt32}
         val schckFromInt32ToInt16 = 
            make {sextdFromLargeToSmall = sextdFromInt32ToInt16,
                  sextdFromSmallToLarge = sextdFromInt16ToInt32}
         val schckFromInt32ToInt32 = sextdFromInt32ToInt32
         val schckFromInt32ToInt64 = sextdFromInt32ToInt64
         val schckFromInt32ToWord8 = 
            make {sextdFromLargeToSmall = sextdFromInt32ToWord8,
                  sextdFromSmallToLarge = sextdFromWord8ToInt32}
         val schckFromInt32ToWord16 = 
            make {sextdFromLargeToSmall = sextdFromInt32ToWord16,
                  sextdFromSmallToLarge = sextdFromWord16ToInt32}
         val schckFromInt32ToWord32 = sextdFromInt32ToWord32
         val schckFromInt32ToWord64 = sextdFromInt32ToWord64
         val schckFromInt64ToInt8 = 
            make {sextdFromLargeToSmall = sextdFromInt64ToInt8,
                  sextdFromSmallToLarge = sextdFromInt8ToInt64}
         val schckFromInt64ToInt16 = 
            make {sextdFromLargeToSmall = sextdFromInt64ToInt16,
                  sextdFromSmallToLarge = sextdFromInt16ToInt64}
         val schckFromInt64ToInt32 = 
            make {sextdFromLargeToSmall = sextdFromInt64ToInt32,
                  sextdFromSmallToLarge = sextdFromInt32ToInt64}
         val schckFromInt64ToInt64 = sextdFromInt64ToInt64
         val schckFromInt64ToWord8 = 
            make {sextdFromLargeToSmall = sextdFromInt64ToWord8,
                  sextdFromSmallToLarge = sextdFromWord8ToInt64}
         val schckFromInt64ToWord16 = 
            make {sextdFromLargeToSmall = sextdFromInt64ToWord16,
                  sextdFromSmallToLarge = sextdFromWord16ToInt64}
         val schckFromInt64ToWord32 = 
            make {sextdFromLargeToSmall = sextdFromInt64ToWord32,
                  sextdFromSmallToLarge = sextdFromWord32ToInt64}
         val schckFromInt64ToWord64 = sextdFromInt64ToWord64

         val schckFromWord8ToInt8 = sextdFromWord8ToInt8
         val schckFromWord8ToInt16 = sextdFromWord8ToInt16
         val schckFromWord8ToInt32 = sextdFromWord8ToInt32
         val schckFromWord8ToInt64 = sextdFromWord8ToInt64
         val schckFromWord8ToWord8 = sextdFromWord8ToWord8
         val schckFromWord8ToWord16 = sextdFromWord8ToWord16
         val schckFromWord8ToWord32 = sextdFromWord8ToWord32
         val schckFromWord8ToWord64 = sextdFromWord8ToWord64
         val schckFromWord16ToInt8 = 
            make {sextdFromLargeToSmall = sextdFromWord16ToInt8,
                  sextdFromSmallToLarge = sextdFromInt8ToWord16}
         val schckFromWord16ToInt16 = sextdFromWord16ToInt16
         val schckFromWord16ToInt32 = sextdFromWord16ToInt32
         val schckFromWord16ToInt64 = sextdFromWord16ToInt64
         val schckFromWord16ToWord8 = 
            make {sextdFromLargeToSmall = sextdFromWord16ToWord8,
                  sextdFromSmallToLarge = sextdFromWord8ToWord16}
         val schckFromWord16ToWord16 = sextdFromWord16ToWord16
         val schckFromWord16ToWord32 = sextdFromWord16ToWord32
         val schckFromWord16ToWord64 = sextdFromWord16ToWord64
         val schckFromWord32ToInt8 = 
            make {sextdFromLargeToSmall = sextdFromWord32ToInt8,
                  sextdFromSmallToLarge = sextdFromInt8ToWord32}
         val schckFromWord32ToInt16 = 
            make {sextdFromLargeToSmall = sextdFromWord32ToInt16,
                  sextdFromSmallToLarge = sextdFromInt16ToWord32}
         val schckFromWord32ToInt32 = sextdFromWord32ToInt32
         val schckFromWord32ToInt64 = sextdFromWord32ToInt64
         val schckFromWord32ToWord8 = 
            make {sextdFromLargeToSmall = sextdFromWord32ToWord8,
                  sextdFromSmallToLarge = sextdFromWord8ToWord32}
         val schckFromWord32ToWord16 = 
            make {sextdFromLargeToSmall = sextdFromWord32ToWord16,
                  sextdFromSmallToLarge = sextdFromWord16ToWord32}
         val schckFromWord32ToWord32 = sextdFromWord32ToWord32
         val schckFromWord32ToWord64 = sextdFromWord32ToWord64
         val schckFromWord64ToInt8 = 
            make {sextdFromLargeToSmall = sextdFromWord64ToInt8,
                  sextdFromSmallToLarge = sextdFromInt8ToWord64}
         val schckFromWord64ToInt16 = 
            make {sextdFromLargeToSmall = sextdFromWord64ToInt16,
                  sextdFromSmallToLarge = sextdFromInt16ToWord64}
         val schckFromWord64ToInt32 = 
            make {sextdFromLargeToSmall = sextdFromWord64ToInt32,
                  sextdFromSmallToLarge = sextdFromInt32ToWord64}
         val schckFromWord64ToInt64 = sextdFromWord64ToInt64
         val schckFromWord64ToWord8 = 
            make {sextdFromLargeToSmall = sextdFromWord64ToWord8,
                  sextdFromSmallToLarge = sextdFromWord8ToWord64}
         val schckFromWord64ToWord16 = 
            make {sextdFromLargeToSmall = sextdFromWord64ToWord16,
                  sextdFromSmallToLarge = sextdFromWord16ToWord64}
         val schckFromWord64ToWord32 = 
            make {sextdFromLargeToSmall = sextdFromWord64ToWord32,
                  sextdFromSmallToLarge = sextdFromWord32ToWord64}
         val schckFromWord64ToWord64 = sextdFromWord64ToWord64
      end

   end

structure Int8 : PRIM_INTEGER =
   struct
      open Int8

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToInt8
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToInt8
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToInt8
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToInt8
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToInt8
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToInt8
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToInt8
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToInt8
      val zextdToInt8 = IntWordConv.zextdFromInt8ToInt8
      val zextdToInt16 = IntWordConv.zextdFromInt8ToInt16
      val zextdToInt32 = IntWordConv.zextdFromInt8ToInt32
      val zextdToInt64 = IntWordConv.zextdFromInt8ToInt64
      val zextdToWord8 = IntWordConv.zextdFromInt8ToWord8
      val zextdToWord16 = IntWordConv.zextdFromInt8ToWord16
      val zextdToWord32 = IntWordConv.zextdFromInt8ToWord32
      val zextdToWord64 = IntWordConv.zextdFromInt8ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToInt8
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToInt8
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToInt8
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToInt8
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToInt8
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToInt8
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToInt8
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToInt8
      val sextdToInt8 = IntWordConv.sextdFromInt8ToInt8
      val sextdToInt16 = IntWordConv.sextdFromInt8ToInt16
      val sextdToInt32 = IntWordConv.sextdFromInt8ToInt32
      val sextdToInt64 = IntWordConv.sextdFromInt8ToInt64
      val sextdToWord8 = IntWordConv.sextdFromInt8ToWord8
      val sextdToWord16 = IntWordConv.sextdFromInt8ToWord16
      val sextdToWord32 = IntWordConv.sextdFromInt8ToWord32
      val sextdToWord64 = IntWordConv.sextdFromInt8ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToInt8
      val castFromInt16 = IntWordConv.castFromInt16ToInt8
      val castFromInt32 = IntWordConv.castFromInt32ToInt8
      val castFromInt64 = IntWordConv.castFromInt64ToInt8
      val castFromWord8 = IntWordConv.castFromWord8ToInt8
      val castFromWord16 = IntWordConv.castFromWord16ToInt8
      val castFromWord32 = IntWordConv.castFromWord32ToInt8
      val castFromWord64 = IntWordConv.castFromWord64ToInt8
      val castToInt8 = IntWordConv.castFromInt8ToInt8
      val castToInt16 = IntWordConv.castFromInt8ToInt16
      val castToInt32 = IntWordConv.castFromInt8ToInt32
      val castToInt64 = IntWordConv.castFromInt8ToInt64
      val castToWord8 = IntWordConv.castFromInt8ToWord8
      val castToWord16 = IntWordConv.castFromInt8ToWord16
      val castToWord32 = IntWordConv.castFromInt8ToWord32
      val castToWord64 = IntWordConv.castFromInt8ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToInt8
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToInt8
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToInt8
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToInt8
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToInt8
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToInt8
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToInt8
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToInt8
      val zchckToInt8 = IntWordConv.zchckFromInt8ToInt8
      val zchckToInt16 = IntWordConv.zchckFromInt8ToInt16
      val zchckToInt32 = IntWordConv.zchckFromInt8ToInt32
      val zchckToInt64 = IntWordConv.zchckFromInt8ToInt64
      val zchckToWord8 = IntWordConv.zchckFromInt8ToWord8
      val zchckToWord16 = IntWordConv.zchckFromInt8ToWord16
      val zchckToWord32 = IntWordConv.zchckFromInt8ToWord32
      val zchckToWord64 = IntWordConv.zchckFromInt8ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToInt8
      val schckFromInt16 = IntWordConv.schckFromInt16ToInt8
      val schckFromInt32 = IntWordConv.schckFromInt32ToInt8
      val schckFromInt64 = IntWordConv.schckFromInt64ToInt8
      val schckFromWord8 = IntWordConv.schckFromWord8ToInt8
      val schckFromWord16 = IntWordConv.schckFromWord16ToInt8
      val schckFromWord32 = IntWordConv.schckFromWord32ToInt8
      val schckFromWord64 = IntWordConv.schckFromWord64ToInt8
      val schckToInt8 = IntWordConv.schckFromInt8ToInt8
      val schckToInt16 = IntWordConv.schckFromInt8ToInt16
      val schckToInt32 = IntWordConv.schckFromInt8ToInt32
      val schckToInt64 = IntWordConv.schckFromInt8ToInt64
      val schckToWord8 = IntWordConv.schckFromInt8ToWord8
      val schckToWord16 = IntWordConv.schckFromInt8ToWord16
      val schckToWord32 = IntWordConv.schckFromInt8ToWord32
      val schckToWord64 = IntWordConv.schckFromInt8ToWord64
   end

structure Int16 : PRIM_INTEGER  =
   struct
      open Int16

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToInt16
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToInt16
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToInt16
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToInt16
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToInt16
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToInt16
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToInt16
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToInt16
      val zextdToInt8 = IntWordConv.zextdFromInt16ToInt8
      val zextdToInt16 = IntWordConv.zextdFromInt16ToInt16
      val zextdToInt32 = IntWordConv.zextdFromInt16ToInt32
      val zextdToInt64 = IntWordConv.zextdFromInt16ToInt64
      val zextdToWord8 = IntWordConv.zextdFromInt16ToWord8
      val zextdToWord16 = IntWordConv.zextdFromInt16ToWord16
      val zextdToWord32 = IntWordConv.zextdFromInt16ToWord32
      val zextdToWord64 = IntWordConv.zextdFromInt16ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToInt16
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToInt16
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToInt16
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToInt16
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToInt16
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToInt16
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToInt16
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToInt16
      val sextdToInt8 = IntWordConv.sextdFromInt16ToInt8
      val sextdToInt16 = IntWordConv.sextdFromInt16ToInt16
      val sextdToInt32 = IntWordConv.sextdFromInt16ToInt32
      val sextdToInt64 = IntWordConv.sextdFromInt16ToInt64
      val sextdToWord8 = IntWordConv.sextdFromInt16ToWord8
      val sextdToWord16 = IntWordConv.sextdFromInt16ToWord16
      val sextdToWord32 = IntWordConv.sextdFromInt16ToWord32
      val sextdToWord64 = IntWordConv.sextdFromInt16ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToInt16
      val castFromInt16 = IntWordConv.castFromInt16ToInt16
      val castFromInt32 = IntWordConv.castFromInt32ToInt16
      val castFromInt64 = IntWordConv.castFromInt64ToInt16
      val castFromWord8 = IntWordConv.castFromWord8ToInt16
      val castFromWord16 = IntWordConv.castFromWord16ToInt16
      val castFromWord32 = IntWordConv.castFromWord32ToInt16
      val castFromWord64 = IntWordConv.castFromWord64ToInt16
      val castToInt8 = IntWordConv.castFromInt16ToInt8
      val castToInt16 = IntWordConv.castFromInt16ToInt16
      val castToInt32 = IntWordConv.castFromInt16ToInt32
      val castToInt64 = IntWordConv.castFromInt16ToInt64
      val castToWord8 = IntWordConv.castFromInt16ToWord8
      val castToWord16 = IntWordConv.castFromInt16ToWord16
      val castToWord32 = IntWordConv.castFromInt16ToWord32
      val castToWord64 = IntWordConv.castFromInt16ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToInt16
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToInt16
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToInt16
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToInt16
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToInt16
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToInt16
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToInt16
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToInt16
      val zchckToInt8 = IntWordConv.zchckFromInt16ToInt8
      val zchckToInt16 = IntWordConv.zchckFromInt16ToInt16
      val zchckToInt32 = IntWordConv.zchckFromInt16ToInt32
      val zchckToInt64 = IntWordConv.zchckFromInt16ToInt64
      val zchckToWord8 = IntWordConv.zchckFromInt16ToWord8
      val zchckToWord16 = IntWordConv.zchckFromInt16ToWord16
      val zchckToWord32 = IntWordConv.zchckFromInt16ToWord32
      val zchckToWord64 = IntWordConv.zchckFromInt16ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToInt16
      val schckFromInt16 = IntWordConv.schckFromInt16ToInt16
      val schckFromInt32 = IntWordConv.schckFromInt32ToInt16
      val schckFromInt64 = IntWordConv.schckFromInt64ToInt16
      val schckFromWord8 = IntWordConv.schckFromWord8ToInt16
      val schckFromWord16 = IntWordConv.schckFromWord16ToInt16
      val schckFromWord32 = IntWordConv.schckFromWord32ToInt16
      val schckFromWord64 = IntWordConv.schckFromWord64ToInt16
      val schckToInt8 = IntWordConv.schckFromInt16ToInt8
      val schckToInt16 = IntWordConv.schckFromInt16ToInt16
      val schckToInt32 = IntWordConv.schckFromInt16ToInt32
      val schckToInt64 = IntWordConv.schckFromInt16ToInt64
      val schckToWord8 = IntWordConv.schckFromInt16ToWord8
      val schckToWord16 = IntWordConv.schckFromInt16ToWord16
      val schckToWord32 = IntWordConv.schckFromInt16ToWord32
      val schckToWord64 = IntWordConv.schckFromInt16ToWord64
   end

structure Int32 : PRIM_INTEGER  =
   struct
      open Int32

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToInt32
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToInt32
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToInt32
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToInt32
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToInt32
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToInt32
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToInt32
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToInt32
      val zextdToInt8 = IntWordConv.zextdFromInt32ToInt8
      val zextdToInt16 = IntWordConv.zextdFromInt32ToInt16
      val zextdToInt32 = IntWordConv.zextdFromInt32ToInt32
      val zextdToInt64 = IntWordConv.zextdFromInt32ToInt64
      val zextdToWord8 = IntWordConv.zextdFromInt32ToWord8
      val zextdToWord16 = IntWordConv.zextdFromInt32ToWord16
      val zextdToWord32 = IntWordConv.zextdFromInt32ToWord32
      val zextdToWord64 = IntWordConv.zextdFromInt32ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToInt32
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToInt32
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToInt32
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToInt32
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToInt32
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToInt32
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToInt32
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToInt32
      val sextdToInt8 = IntWordConv.sextdFromInt32ToInt8
      val sextdToInt16 = IntWordConv.sextdFromInt32ToInt16
      val sextdToInt32 = IntWordConv.sextdFromInt32ToInt32
      val sextdToInt64 = IntWordConv.sextdFromInt32ToInt64
      val sextdToWord8 = IntWordConv.sextdFromInt32ToWord8
      val sextdToWord16 = IntWordConv.sextdFromInt32ToWord16
      val sextdToWord32 = IntWordConv.sextdFromInt32ToWord32
      val sextdToWord64 = IntWordConv.sextdFromInt32ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToInt32
      val castFromInt16 = IntWordConv.castFromInt16ToInt32
      val castFromInt32 = IntWordConv.castFromInt32ToInt32
      val castFromInt64 = IntWordConv.castFromInt64ToInt32
      val castFromWord8 = IntWordConv.castFromWord8ToInt32
      val castFromWord16 = IntWordConv.castFromWord16ToInt32
      val castFromWord32 = IntWordConv.castFromWord32ToInt32
      val castFromWord64 = IntWordConv.castFromWord64ToInt32
      val castToInt8 = IntWordConv.castFromInt32ToInt8
      val castToInt16 = IntWordConv.castFromInt32ToInt16
      val castToInt32 = IntWordConv.castFromInt32ToInt32
      val castToInt64 = IntWordConv.castFromInt32ToInt64
      val castToWord8 = IntWordConv.castFromInt32ToWord8
      val castToWord16 = IntWordConv.castFromInt32ToWord16
      val castToWord32 = IntWordConv.castFromInt32ToWord32
      val castToWord64 = IntWordConv.castFromInt32ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToInt32
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToInt32
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToInt32
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToInt32
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToInt32
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToInt32
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToInt32
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToInt32
      val zchckToInt8 = IntWordConv.zchckFromInt32ToInt8
      val zchckToInt16 = IntWordConv.zchckFromInt32ToInt16
      val zchckToInt32 = IntWordConv.zchckFromInt32ToInt32
      val zchckToInt64 = IntWordConv.zchckFromInt32ToInt64
      val zchckToWord8 = IntWordConv.zchckFromInt32ToWord8
      val zchckToWord16 = IntWordConv.zchckFromInt32ToWord16
      val zchckToWord32 = IntWordConv.zchckFromInt32ToWord32
      val zchckToWord64 = IntWordConv.zchckFromInt32ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToInt32
      val schckFromInt16 = IntWordConv.schckFromInt16ToInt32
      val schckFromInt32 = IntWordConv.schckFromInt32ToInt32
      val schckFromInt64 = IntWordConv.schckFromInt64ToInt32
      val schckFromWord8 = IntWordConv.schckFromWord8ToInt32
      val schckFromWord16 = IntWordConv.schckFromWord16ToInt32
      val schckFromWord32 = IntWordConv.schckFromWord32ToInt32
      val schckFromWord64 = IntWordConv.schckFromWord64ToInt32
      val schckToInt8 = IntWordConv.schckFromInt32ToInt8
      val schckToInt16 = IntWordConv.schckFromInt32ToInt16
      val schckToInt32 = IntWordConv.schckFromInt32ToInt32
      val schckToInt64 = IntWordConv.schckFromInt32ToInt64
      val schckToWord8 = IntWordConv.schckFromInt32ToWord8
      val schckToWord16 = IntWordConv.schckFromInt32ToWord16
      val schckToWord32 = IntWordConv.schckFromInt32ToWord32
      val schckToWord64 = IntWordConv.schckFromInt32ToWord64
   end

structure Int64 : PRIM_INTEGER  =
   struct
      open Int64

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToInt64
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToInt64
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToInt64
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToInt64
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToInt64
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToInt64
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToInt64
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToInt64
      val zextdToInt8 = IntWordConv.zextdFromInt64ToInt8
      val zextdToInt16 = IntWordConv.zextdFromInt64ToInt16
      val zextdToInt32 = IntWordConv.zextdFromInt64ToInt32
      val zextdToInt64 = IntWordConv.zextdFromInt64ToInt64
      val zextdToWord8 = IntWordConv.zextdFromInt64ToWord8
      val zextdToWord16 = IntWordConv.zextdFromInt64ToWord16
      val zextdToWord32 = IntWordConv.zextdFromInt64ToWord32
      val zextdToWord64 = IntWordConv.zextdFromInt64ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToInt64
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToInt64
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToInt64
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToInt64
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToInt64
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToInt64
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToInt64
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToInt64
      val sextdToInt8 = IntWordConv.sextdFromInt64ToInt8
      val sextdToInt16 = IntWordConv.sextdFromInt64ToInt16
      val sextdToInt32 = IntWordConv.sextdFromInt64ToInt32
      val sextdToInt64 = IntWordConv.sextdFromInt64ToInt64
      val sextdToWord8 = IntWordConv.sextdFromInt64ToWord8
      val sextdToWord16 = IntWordConv.sextdFromInt64ToWord16
      val sextdToWord32 = IntWordConv.sextdFromInt64ToWord32
      val sextdToWord64 = IntWordConv.sextdFromInt64ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToInt64
      val castFromInt16 = IntWordConv.castFromInt16ToInt64
      val castFromInt32 = IntWordConv.castFromInt32ToInt64
      val castFromInt64 = IntWordConv.castFromInt64ToInt64
      val castFromWord8 = IntWordConv.castFromWord8ToInt64
      val castFromWord16 = IntWordConv.castFromWord16ToInt64
      val castFromWord32 = IntWordConv.castFromWord32ToInt64
      val castFromWord64 = IntWordConv.castFromWord64ToInt64
      val castToInt8 = IntWordConv.castFromInt64ToInt8
      val castToInt16 = IntWordConv.castFromInt64ToInt16
      val castToInt32 = IntWordConv.castFromInt64ToInt32
      val castToInt64 = IntWordConv.castFromInt64ToInt64
      val castToWord8 = IntWordConv.castFromInt64ToWord8
      val castToWord16 = IntWordConv.castFromInt64ToWord16
      val castToWord32 = IntWordConv.castFromInt64ToWord32
      val castToWord64 = IntWordConv.castFromInt64ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToInt64
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToInt64
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToInt64
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToInt64
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToInt64
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToInt64
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToInt64
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToInt64
      val zchckToInt8 = IntWordConv.zchckFromInt64ToInt8
      val zchckToInt16 = IntWordConv.zchckFromInt64ToInt16
      val zchckToInt32 = IntWordConv.zchckFromInt64ToInt32
      val zchckToInt64 = IntWordConv.zchckFromInt64ToInt64
      val zchckToWord8 = IntWordConv.zchckFromInt64ToWord8
      val zchckToWord16 = IntWordConv.zchckFromInt64ToWord16
      val zchckToWord32 = IntWordConv.zchckFromInt64ToWord32
      val zchckToWord64 = IntWordConv.zchckFromInt64ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToInt64
      val schckFromInt16 = IntWordConv.schckFromInt16ToInt64
      val schckFromInt32 = IntWordConv.schckFromInt32ToInt64
      val schckFromInt64 = IntWordConv.schckFromInt64ToInt64
      val schckFromWord8 = IntWordConv.schckFromWord8ToInt64
      val schckFromWord16 = IntWordConv.schckFromWord16ToInt64
      val schckFromWord32 = IntWordConv.schckFromWord32ToInt64
      val schckFromWord64 = IntWordConv.schckFromWord64ToInt64
      val schckToInt8 = IntWordConv.schckFromInt64ToInt8
      val schckToInt16 = IntWordConv.schckFromInt64ToInt16
      val schckToInt32 = IntWordConv.schckFromInt64ToInt32
      val schckToInt64 = IntWordConv.schckFromInt64ToInt64
      val schckToWord8 = IntWordConv.schckFromInt64ToWord8
      val schckToWord16 = IntWordConv.schckFromInt64ToWord16
      val schckToWord32 = IntWordConv.schckFromInt64ToWord32
      val schckToWord64 = IntWordConv.schckFromInt64ToWord64
   end

structure Word8 : PRIM_WORD  =
   struct
      open Word8

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToWord8
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToWord8
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToWord8
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToWord8
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToWord8
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToWord8
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToWord8
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToWord8
      val zextdToInt8 = IntWordConv.zextdFromWord8ToInt8
      val zextdToInt16 = IntWordConv.zextdFromWord8ToInt16
      val zextdToInt32 = IntWordConv.zextdFromWord8ToInt32
      val zextdToInt64 = IntWordConv.zextdFromWord8ToInt64
      val zextdToWord8 = IntWordConv.zextdFromWord8ToWord8
      val zextdToWord16 = IntWordConv.zextdFromWord8ToWord16
      val zextdToWord32 = IntWordConv.zextdFromWord8ToWord32
      val zextdToWord64 = IntWordConv.zextdFromWord8ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToWord8
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToWord8
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToWord8
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToWord8
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToWord8
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToWord8
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToWord8
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToWord8
      val sextdToInt8 = IntWordConv.sextdFromWord8ToInt8
      val sextdToInt16 = IntWordConv.sextdFromWord8ToInt16
      val sextdToInt32 = IntWordConv.sextdFromWord8ToInt32
      val sextdToInt64 = IntWordConv.sextdFromWord8ToInt64
      val sextdToWord8 = IntWordConv.sextdFromWord8ToWord8
      val sextdToWord16 = IntWordConv.sextdFromWord8ToWord16
      val sextdToWord32 = IntWordConv.sextdFromWord8ToWord32
      val sextdToWord64 = IntWordConv.sextdFromWord8ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToWord8
      val castFromInt16 = IntWordConv.castFromInt16ToWord8
      val castFromInt32 = IntWordConv.castFromInt32ToWord8
      val castFromInt64 = IntWordConv.castFromInt64ToWord8
      val castFromWord8 = IntWordConv.castFromWord8ToWord8
      val castFromWord16 = IntWordConv.castFromWord16ToWord8
      val castFromWord32 = IntWordConv.castFromWord32ToWord8
      val castFromWord64 = IntWordConv.castFromWord64ToWord8
      val castToInt8 = IntWordConv.castFromWord8ToInt8
      val castToInt16 = IntWordConv.castFromWord8ToInt16
      val castToInt32 = IntWordConv.castFromWord8ToInt32
      val castToInt64 = IntWordConv.castFromWord8ToInt64
      val castToWord8 = IntWordConv.castFromWord8ToWord8
      val castToWord16 = IntWordConv.castFromWord8ToWord16
      val castToWord32 = IntWordConv.castFromWord8ToWord32
      val castToWord64 = IntWordConv.castFromWord8ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToWord8
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToWord8
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToWord8
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToWord8
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToWord8
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToWord8
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToWord8
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToWord8
      val zchckToInt8 = IntWordConv.zchckFromWord8ToInt8
      val zchckToInt16 = IntWordConv.zchckFromWord8ToInt16
      val zchckToInt32 = IntWordConv.zchckFromWord8ToInt32
      val zchckToInt64 = IntWordConv.zchckFromWord8ToInt64
      val zchckToWord8 = IntWordConv.zchckFromWord8ToWord8
      val zchckToWord16 = IntWordConv.zchckFromWord8ToWord16
      val zchckToWord32 = IntWordConv.zchckFromWord8ToWord32
      val zchckToWord64 = IntWordConv.zchckFromWord8ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToWord8
      val schckFromInt16 = IntWordConv.schckFromInt16ToWord8
      val schckFromInt32 = IntWordConv.schckFromInt32ToWord8
      val schckFromInt64 = IntWordConv.schckFromInt64ToWord8
      val schckFromWord8 = IntWordConv.schckFromWord8ToWord8
      val schckFromWord16 = IntWordConv.schckFromWord16ToWord8
      val schckFromWord32 = IntWordConv.schckFromWord32ToWord8
      val schckFromWord64 = IntWordConv.schckFromWord64ToWord8
      val schckToInt8 = IntWordConv.schckFromWord8ToInt8
      val schckToInt16 = IntWordConv.schckFromWord8ToInt16
      val schckToInt32 = IntWordConv.schckFromWord8ToInt32
      val schckToInt64 = IntWordConv.schckFromWord8ToInt64
      val schckToWord8 = IntWordConv.schckFromWord8ToWord8
      val schckToWord16 = IntWordConv.schckFromWord8ToWord16
      val schckToWord32 = IntWordConv.schckFromWord8ToWord32
      val schckToWord64 = IntWordConv.schckFromWord8ToWord64
   end

structure Word16 : PRIM_WORD =
   struct
      open Word16

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToWord16
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToWord16
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToWord16
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToWord16
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToWord16
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToWord16
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToWord16
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToWord16
      val zextdToInt8 = IntWordConv.zextdFromWord16ToInt8
      val zextdToInt16 = IntWordConv.zextdFromWord16ToInt16
      val zextdToInt32 = IntWordConv.zextdFromWord16ToInt32
      val zextdToInt64 = IntWordConv.zextdFromWord16ToInt64
      val zextdToWord8 = IntWordConv.zextdFromWord16ToWord8
      val zextdToWord16 = IntWordConv.zextdFromWord16ToWord16
      val zextdToWord32 = IntWordConv.zextdFromWord16ToWord32
      val zextdToWord64 = IntWordConv.zextdFromWord16ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToWord16
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToWord16
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToWord16
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToWord16
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToWord16
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToWord16
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToWord16
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToWord16
      val sextdToInt8 = IntWordConv.sextdFromWord16ToInt8
      val sextdToInt16 = IntWordConv.sextdFromWord16ToInt16
      val sextdToInt32 = IntWordConv.sextdFromWord16ToInt32
      val sextdToInt64 = IntWordConv.sextdFromWord16ToInt64
      val sextdToWord8 = IntWordConv.sextdFromWord16ToWord8
      val sextdToWord16 = IntWordConv.sextdFromWord16ToWord16
      val sextdToWord32 = IntWordConv.sextdFromWord16ToWord32
      val sextdToWord64 = IntWordConv.sextdFromWord16ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToWord16
      val castFromInt16 = IntWordConv.castFromInt16ToWord16
      val castFromInt32 = IntWordConv.castFromInt32ToWord16
      val castFromInt64 = IntWordConv.castFromInt64ToWord16
      val castFromWord8 = IntWordConv.castFromWord8ToWord16
      val castFromWord16 = IntWordConv.castFromWord16ToWord16
      val castFromWord32 = IntWordConv.castFromWord32ToWord16
      val castFromWord64 = IntWordConv.castFromWord64ToWord16
      val castToInt8 = IntWordConv.castFromWord16ToInt8
      val castToInt16 = IntWordConv.castFromWord16ToInt16
      val castToInt32 = IntWordConv.castFromWord16ToInt32
      val castToInt64 = IntWordConv.castFromWord16ToInt64
      val castToWord8 = IntWordConv.castFromWord16ToWord8
      val castToWord16 = IntWordConv.castFromWord16ToWord16
      val castToWord32 = IntWordConv.castFromWord16ToWord32
      val castToWord64 = IntWordConv.castFromWord16ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToWord16
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToWord16
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToWord16
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToWord16
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToWord16
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToWord16
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToWord16
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToWord16
      val zchckToInt8 = IntWordConv.zchckFromWord16ToInt8
      val zchckToInt16 = IntWordConv.zchckFromWord16ToInt16
      val zchckToInt32 = IntWordConv.zchckFromWord16ToInt32
      val zchckToInt64 = IntWordConv.zchckFromWord16ToInt64
      val zchckToWord8 = IntWordConv.zchckFromWord16ToWord8
      val zchckToWord16 = IntWordConv.zchckFromWord16ToWord16
      val zchckToWord32 = IntWordConv.zchckFromWord16ToWord32
      val zchckToWord64 = IntWordConv.zchckFromWord16ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToWord16
      val schckFromInt16 = IntWordConv.schckFromInt16ToWord16
      val schckFromInt32 = IntWordConv.schckFromInt32ToWord16
      val schckFromInt64 = IntWordConv.schckFromInt64ToWord16
      val schckFromWord8 = IntWordConv.schckFromWord8ToWord16
      val schckFromWord16 = IntWordConv.schckFromWord16ToWord16
      val schckFromWord32 = IntWordConv.schckFromWord32ToWord16
      val schckFromWord64 = IntWordConv.schckFromWord64ToWord16
      val schckToInt8 = IntWordConv.schckFromWord16ToInt8
      val schckToInt16 = IntWordConv.schckFromWord16ToInt16
      val schckToInt32 = IntWordConv.schckFromWord16ToInt32
      val schckToInt64 = IntWordConv.schckFromWord16ToInt64
      val schckToWord8 = IntWordConv.schckFromWord16ToWord8
      val schckToWord16 = IntWordConv.schckFromWord16ToWord16
      val schckToWord32 = IntWordConv.schckFromWord16ToWord32
      val schckToWord64 = IntWordConv.schckFromWord16ToWord64
   end

structure Word32 : PRIM_WORD =
   struct
      open Word32

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToWord32
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToWord32
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToWord32
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToWord32
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToWord32
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToWord32
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToWord32
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToWord32
      val zextdToInt8 = IntWordConv.zextdFromWord32ToInt8
      val zextdToInt16 = IntWordConv.zextdFromWord32ToInt16
      val zextdToInt32 = IntWordConv.zextdFromWord32ToInt32
      val zextdToInt64 = IntWordConv.zextdFromWord32ToInt64
      val zextdToWord8 = IntWordConv.zextdFromWord32ToWord8
      val zextdToWord16 = IntWordConv.zextdFromWord32ToWord16
      val zextdToWord32 = IntWordConv.zextdFromWord32ToWord32
      val zextdToWord64 = IntWordConv.zextdFromWord32ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToWord32
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToWord32
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToWord32
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToWord32
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToWord32
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToWord32
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToWord32
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToWord32
      val sextdToInt8 = IntWordConv.sextdFromWord32ToInt8
      val sextdToInt16 = IntWordConv.sextdFromWord32ToInt16
      val sextdToInt32 = IntWordConv.sextdFromWord32ToInt32
      val sextdToInt64 = IntWordConv.sextdFromWord32ToInt64
      val sextdToWord8 = IntWordConv.sextdFromWord32ToWord8
      val sextdToWord16 = IntWordConv.sextdFromWord32ToWord16
      val sextdToWord32 = IntWordConv.sextdFromWord32ToWord32
      val sextdToWord64 = IntWordConv.sextdFromWord32ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToWord32
      val castFromInt16 = IntWordConv.castFromInt16ToWord32
      val castFromInt32 = IntWordConv.castFromInt32ToWord32
      val castFromInt64 = IntWordConv.castFromInt64ToWord32
      val castFromWord8 = IntWordConv.castFromWord8ToWord32
      val castFromWord16 = IntWordConv.castFromWord16ToWord32
      val castFromWord32 = IntWordConv.castFromWord32ToWord32
      val castFromWord64 = IntWordConv.castFromWord64ToWord32
      val castToInt8 = IntWordConv.castFromWord32ToInt8
      val castToInt16 = IntWordConv.castFromWord32ToInt16
      val castToInt32 = IntWordConv.castFromWord32ToInt32
      val castToInt64 = IntWordConv.castFromWord32ToInt64
      val castToWord8 = IntWordConv.castFromWord32ToWord8
      val castToWord16 = IntWordConv.castFromWord32ToWord16
      val castToWord32 = IntWordConv.castFromWord32ToWord32
      val castToWord64 = IntWordConv.castFromWord32ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToWord32
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToWord32
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToWord32
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToWord32
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToWord32
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToWord32
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToWord32
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToWord32
      val zchckToInt8 = IntWordConv.zchckFromWord32ToInt8
      val zchckToInt16 = IntWordConv.zchckFromWord32ToInt16
      val zchckToInt32 = IntWordConv.zchckFromWord32ToInt32
      val zchckToInt64 = IntWordConv.zchckFromWord32ToInt64
      val zchckToWord8 = IntWordConv.zchckFromWord32ToWord8
      val zchckToWord16 = IntWordConv.zchckFromWord32ToWord16
      val zchckToWord32 = IntWordConv.zchckFromWord32ToWord32
      val zchckToWord64 = IntWordConv.zchckFromWord32ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToWord32
      val schckFromInt16 = IntWordConv.schckFromInt16ToWord32
      val schckFromInt32 = IntWordConv.schckFromInt32ToWord32
      val schckFromInt64 = IntWordConv.schckFromInt64ToWord32
      val schckFromWord8 = IntWordConv.schckFromWord8ToWord32
      val schckFromWord16 = IntWordConv.schckFromWord16ToWord32
      val schckFromWord32 = IntWordConv.schckFromWord32ToWord32
      val schckFromWord64 = IntWordConv.schckFromWord64ToWord32
      val schckToInt8 = IntWordConv.schckFromWord32ToInt8
      val schckToInt16 = IntWordConv.schckFromWord32ToInt16
      val schckToInt32 = IntWordConv.schckFromWord32ToInt32
      val schckToInt64 = IntWordConv.schckFromWord32ToInt64
      val schckToWord8 = IntWordConv.schckFromWord32ToWord8
      val schckToWord16 = IntWordConv.schckFromWord32ToWord16
      val schckToWord32 = IntWordConv.schckFromWord32ToWord32
      val schckToWord64 = IntWordConv.schckFromWord32ToWord64
   end

structure Word64 : PRIM_WORD =
   struct
      open Word64

      val zextdFromInt8 = IntWordConv.zextdFromInt8ToWord64
      val zextdFromInt16 = IntWordConv.zextdFromInt16ToWord64
      val zextdFromInt32 = IntWordConv.zextdFromInt32ToWord64
      val zextdFromInt64 = IntWordConv.zextdFromInt64ToWord64
      val zextdFromWord8 = IntWordConv.zextdFromWord8ToWord64
      val zextdFromWord16 = IntWordConv.zextdFromWord16ToWord64
      val zextdFromWord32 = IntWordConv.zextdFromWord32ToWord64
      val zextdFromWord64 = IntWordConv.zextdFromWord64ToWord64
      val zextdToInt8 = IntWordConv.zextdFromWord64ToInt8
      val zextdToInt16 = IntWordConv.zextdFromWord64ToInt16
      val zextdToInt32 = IntWordConv.zextdFromWord64ToInt32
      val zextdToInt64 = IntWordConv.zextdFromWord64ToInt64
      val zextdToWord8 = IntWordConv.zextdFromWord64ToWord8
      val zextdToWord16 = IntWordConv.zextdFromWord64ToWord16
      val zextdToWord32 = IntWordConv.zextdFromWord64ToWord32
      val zextdToWord64 = IntWordConv.zextdFromWord64ToWord64

      val sextdFromInt8 = IntWordConv.sextdFromInt8ToWord64
      val sextdFromInt16 = IntWordConv.sextdFromInt16ToWord64
      val sextdFromInt32 = IntWordConv.sextdFromInt32ToWord64
      val sextdFromInt64 = IntWordConv.sextdFromInt64ToWord64
      val sextdFromWord8 = IntWordConv.sextdFromWord8ToWord64
      val sextdFromWord16 = IntWordConv.sextdFromWord16ToWord64
      val sextdFromWord32 = IntWordConv.sextdFromWord32ToWord64
      val sextdFromWord64 = IntWordConv.sextdFromWord64ToWord64
      val sextdToInt8 = IntWordConv.sextdFromWord64ToInt8
      val sextdToInt16 = IntWordConv.sextdFromWord64ToInt16
      val sextdToInt32 = IntWordConv.sextdFromWord64ToInt32
      val sextdToInt64 = IntWordConv.sextdFromWord64ToInt64
      val sextdToWord8 = IntWordConv.sextdFromWord64ToWord8
      val sextdToWord16 = IntWordConv.sextdFromWord64ToWord16
      val sextdToWord32 = IntWordConv.sextdFromWord64ToWord32
      val sextdToWord64 = IntWordConv.sextdFromWord64ToWord64

      val castFromInt8 = IntWordConv.castFromInt8ToWord64
      val castFromInt16 = IntWordConv.castFromInt16ToWord64
      val castFromInt32 = IntWordConv.castFromInt32ToWord64
      val castFromInt64 = IntWordConv.castFromInt64ToWord64
      val castFromWord8 = IntWordConv.castFromWord8ToWord64
      val castFromWord16 = IntWordConv.castFromWord16ToWord64
      val castFromWord32 = IntWordConv.castFromWord32ToWord64
      val castFromWord64 = IntWordConv.castFromWord64ToWord64
      val castToInt8 = IntWordConv.castFromWord64ToInt8
      val castToInt16 = IntWordConv.castFromWord64ToInt16
      val castToInt32 = IntWordConv.castFromWord64ToInt32
      val castToInt64 = IntWordConv.castFromWord64ToInt64
      val castToWord8 = IntWordConv.castFromWord64ToWord8
      val castToWord16 = IntWordConv.castFromWord64ToWord16
      val castToWord32 = IntWordConv.castFromWord64ToWord32
      val castToWord64 = IntWordConv.castFromWord64ToWord64

      val zchckFromInt8 = IntWordConv.zchckFromInt8ToWord64
      val zchckFromInt16 = IntWordConv.zchckFromInt16ToWord64
      val zchckFromInt32 = IntWordConv.zchckFromInt32ToWord64
      val zchckFromInt64 = IntWordConv.zchckFromInt64ToWord64
      val zchckFromWord8 = IntWordConv.zchckFromWord8ToWord64
      val zchckFromWord16 = IntWordConv.zchckFromWord16ToWord64
      val zchckFromWord32 = IntWordConv.zchckFromWord32ToWord64
      val zchckFromWord64 = IntWordConv.zchckFromWord64ToWord64
      val zchckToInt8 = IntWordConv.zchckFromWord64ToInt8
      val zchckToInt16 = IntWordConv.zchckFromWord64ToInt16
      val zchckToInt32 = IntWordConv.zchckFromWord64ToInt32
      val zchckToInt64 = IntWordConv.zchckFromWord64ToInt64
      val zchckToWord8 = IntWordConv.zchckFromWord64ToWord8
      val zchckToWord16 = IntWordConv.zchckFromWord64ToWord16
      val zchckToWord32 = IntWordConv.zchckFromWord64ToWord32
      val zchckToWord64 = IntWordConv.zchckFromWord64ToWord64

      val schckFromInt8 = IntWordConv.schckFromInt8ToWord64
      val schckFromInt16 = IntWordConv.schckFromInt16ToWord64
      val schckFromInt32 = IntWordConv.schckFromInt32ToWord64
      val schckFromInt64 = IntWordConv.schckFromInt64ToWord64
      val schckFromWord8 = IntWordConv.schckFromWord8ToWord64
      val schckFromWord16 = IntWordConv.schckFromWord16ToWord64
      val schckFromWord32 = IntWordConv.schckFromWord32ToWord64
      val schckFromWord64 = IntWordConv.schckFromWord64ToWord64
      val schckToInt8 = IntWordConv.schckFromWord64ToInt8
      val schckToInt16 = IntWordConv.schckFromWord64ToInt16
      val schckToInt32 = IntWordConv.schckFromWord64ToInt32
      val schckToInt64 = IntWordConv.schckFromWord64ToInt64
      val schckToWord8 = IntWordConv.schckFromWord64ToWord8
      val schckToWord16 = IntWordConv.schckFromWord64ToWord16
      val schckToWord32 = IntWordConv.schckFromWord64ToWord32
      val schckToWord64 = IntWordConv.schckFromWord64ToWord64
   end

end
