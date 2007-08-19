(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

signature PRIM_INTWORD_CONV =
   sig
      (* identity *)
      val idFromInt8ToInt8: Primitive.Int8.int -> Primitive.Int8.int
      val idFromInt8ToWord8: Primitive.Int8.int -> Primitive.Word8.word
      val idFromInt16ToInt16: Primitive.Int16.int -> Primitive.Int16.int
      val idFromInt16ToWord16: Primitive.Int16.int -> Primitive.Word16.word
      val idFromInt32ToInt32: Primitive.Int32.int -> Primitive.Int32.int
      val idFromInt32ToWord32: Primitive.Int32.int -> Primitive.Word32.word
      val idFromInt64ToInt64: Primitive.Int64.int -> Primitive.Int64.int
      val idFromInt64ToWord64: Primitive.Int64.int -> Primitive.Word64.word
      val idFromWord8ToInt8: Primitive.Word8.word -> Primitive.Int8.int
      val idFromWord8ToWord8: Primitive.Word8.word -> Primitive.Word8.word
      val idFromWord16ToInt16: Primitive.Word16.word -> Primitive.Int16.int
      val idFromWord16ToWord16: Primitive.Word16.word -> Primitive.Word16.word
      val idFromWord32ToInt32: Primitive.Word32.word -> Primitive.Int32.int
      val idFromWord32ToWord32: Primitive.Word32.word -> Primitive.Word32.word
      val idFromWord64ToInt64: Primitive.Word64.word -> Primitive.Int64.int
      val idFromWord64ToWord64: Primitive.Word64.word -> Primitive.Word64.word

      (* zero-extend or low-bits *)
      val zextdFromInt8ToInt8: Primitive.Int8.int -> Primitive.Int8.int
      val zextdFromInt8ToInt16: Primitive.Int8.int -> Primitive.Int16.int
      val zextdFromInt8ToInt32: Primitive.Int8.int -> Primitive.Int32.int
      val zextdFromInt8ToInt64: Primitive.Int8.int -> Primitive.Int64.int
      val zextdFromInt8ToWord8: Primitive.Int8.int -> Primitive.Word8.word
      val zextdFromInt8ToWord16: Primitive.Int8.int -> Primitive.Word16.word
      val zextdFromInt8ToWord32: Primitive.Int8.int -> Primitive.Word32.word
      val zextdFromInt8ToWord64: Primitive.Int8.int -> Primitive.Word64.word

      val zextdFromInt16ToInt8: Primitive.Int16.int -> Primitive.Int8.int
      val zextdFromInt16ToInt16: Primitive.Int16.int -> Primitive.Int16.int
      val zextdFromInt16ToInt32: Primitive.Int16.int -> Primitive.Int32.int
      val zextdFromInt16ToInt64: Primitive.Int16.int -> Primitive.Int64.int
      val zextdFromInt16ToWord8: Primitive.Int16.int -> Primitive.Word8.word
      val zextdFromInt16ToWord16: Primitive.Int16.int -> Primitive.Word16.word
      val zextdFromInt16ToWord32: Primitive.Int16.int -> Primitive.Word32.word
      val zextdFromInt16ToWord64: Primitive.Int16.int -> Primitive.Word64.word

      val zextdFromInt32ToInt8: Primitive.Int32.int -> Primitive.Int8.int
      val zextdFromInt32ToInt16: Primitive.Int32.int -> Primitive.Int16.int
      val zextdFromInt32ToInt32: Primitive.Int32.int -> Primitive.Int32.int
      val zextdFromInt32ToInt64: Primitive.Int32.int -> Primitive.Int64.int
      val zextdFromInt32ToWord8: Primitive.Int32.int -> Primitive.Word8.word
      val zextdFromInt32ToWord16: Primitive.Int32.int -> Primitive.Word16.word
      val zextdFromInt32ToWord32: Primitive.Int32.int -> Primitive.Word32.word
      val zextdFromInt32ToWord64: Primitive.Int32.int -> Primitive.Word64.word

      val zextdFromInt64ToInt8: Primitive.Int64.int -> Primitive.Int8.int
      val zextdFromInt64ToInt16: Primitive.Int64.int -> Primitive.Int16.int
      val zextdFromInt64ToInt32: Primitive.Int64.int -> Primitive.Int32.int
      val zextdFromInt64ToInt64: Primitive.Int64.int -> Primitive.Int64.int
      val zextdFromInt64ToWord8: Primitive.Int64.int -> Primitive.Word8.word
      val zextdFromInt64ToWord16: Primitive.Int64.int -> Primitive.Word16.word
      val zextdFromInt64ToWord32: Primitive.Int64.int -> Primitive.Word32.word
      val zextdFromInt64ToWord64: Primitive.Int64.int -> Primitive.Word64.word

      val zextdFromWord8ToInt8: Primitive.Word8.word -> Primitive.Int8.int
      val zextdFromWord8ToInt16: Primitive.Word8.word -> Primitive.Int16.int
      val zextdFromWord8ToInt32: Primitive.Word8.word -> Primitive.Int32.int
      val zextdFromWord8ToInt64: Primitive.Word8.word -> Primitive.Int64.int
      val zextdFromWord8ToWord8: Primitive.Word8.word -> Primitive.Word8.word
      val zextdFromWord8ToWord16: Primitive.Word8.word -> Primitive.Word16.word
      val zextdFromWord8ToWord32: Primitive.Word8.word -> Primitive.Word32.word
      val zextdFromWord8ToWord64: Primitive.Word8.word -> Primitive.Word64.word

      val zextdFromWord16ToInt8: Primitive.Word16.word -> Primitive.Int8.int
      val zextdFromWord16ToInt16: Primitive.Word16.word -> Primitive.Int16.int
      val zextdFromWord16ToInt32: Primitive.Word16.word -> Primitive.Int32.int
      val zextdFromWord16ToInt64: Primitive.Word16.word -> Primitive.Int64.int
      val zextdFromWord16ToWord8: Primitive.Word16.word -> Primitive.Word8.word
      val zextdFromWord16ToWord16: Primitive.Word16.word -> Primitive.Word16.word
      val zextdFromWord16ToWord32: Primitive.Word16.word -> Primitive.Word32.word
      val zextdFromWord16ToWord64: Primitive.Word16.word -> Primitive.Word64.word

      val zextdFromWord32ToInt8: Primitive.Word32.word -> Primitive.Int8.int
      val zextdFromWord32ToInt16: Primitive.Word32.word -> Primitive.Int16.int
      val zextdFromWord32ToInt32: Primitive.Word32.word -> Primitive.Int32.int
      val zextdFromWord32ToInt64: Primitive.Word32.word -> Primitive.Int64.int
      val zextdFromWord32ToWord8: Primitive.Word32.word -> Primitive.Word8.word
      val zextdFromWord32ToWord16: Primitive.Word32.word -> Primitive.Word16.word
      val zextdFromWord32ToWord32: Primitive.Word32.word -> Primitive.Word32.word
      val zextdFromWord32ToWord64: Primitive.Word32.word -> Primitive.Word64.word

      val zextdFromWord64ToInt8: Primitive.Word64.word -> Primitive.Int8.int
      val zextdFromWord64ToInt16: Primitive.Word64.word -> Primitive.Int16.int
      val zextdFromWord64ToInt32: Primitive.Word64.word -> Primitive.Int32.int
      val zextdFromWord64ToInt64: Primitive.Word64.word -> Primitive.Int64.int
      val zextdFromWord64ToWord8: Primitive.Word64.word -> Primitive.Word8.word
      val zextdFromWord64ToWord16: Primitive.Word64.word -> Primitive.Word16.word
      val zextdFromWord64ToWord32: Primitive.Word64.word -> Primitive.Word32.word
      val zextdFromWord64ToWord64: Primitive.Word64.word -> Primitive.Word64.word

      (* sign-extend or low-bits *)
      val sextdFromInt8ToInt8: Primitive.Int8.int -> Primitive.Int8.int
      val sextdFromInt8ToInt16: Primitive.Int8.int -> Primitive.Int16.int
      val sextdFromInt8ToInt32: Primitive.Int8.int -> Primitive.Int32.int
      val sextdFromInt8ToInt64: Primitive.Int8.int -> Primitive.Int64.int
      val sextdFromInt8ToWord8: Primitive.Int8.int -> Primitive.Word8.word
      val sextdFromInt8ToWord16: Primitive.Int8.int -> Primitive.Word16.word
      val sextdFromInt8ToWord32: Primitive.Int8.int -> Primitive.Word32.word
      val sextdFromInt8ToWord64: Primitive.Int8.int -> Primitive.Word64.word

      val sextdFromInt16ToInt8: Primitive.Int16.int -> Primitive.Int8.int
      val sextdFromInt16ToInt16: Primitive.Int16.int -> Primitive.Int16.int
      val sextdFromInt16ToInt32: Primitive.Int16.int -> Primitive.Int32.int
      val sextdFromInt16ToInt64: Primitive.Int16.int -> Primitive.Int64.int
      val sextdFromInt16ToWord8: Primitive.Int16.int -> Primitive.Word8.word
      val sextdFromInt16ToWord16: Primitive.Int16.int -> Primitive.Word16.word
      val sextdFromInt16ToWord32: Primitive.Int16.int -> Primitive.Word32.word
      val sextdFromInt16ToWord64: Primitive.Int16.int -> Primitive.Word64.word

      val sextdFromInt32ToInt8: Primitive.Int32.int -> Primitive.Int8.int
      val sextdFromInt32ToInt16: Primitive.Int32.int -> Primitive.Int16.int
      val sextdFromInt32ToInt32: Primitive.Int32.int -> Primitive.Int32.int
      val sextdFromInt32ToInt64: Primitive.Int32.int -> Primitive.Int64.int
      val sextdFromInt32ToWord8: Primitive.Int32.int -> Primitive.Word8.word
      val sextdFromInt32ToWord16: Primitive.Int32.int -> Primitive.Word16.word
      val sextdFromInt32ToWord32: Primitive.Int32.int -> Primitive.Word32.word
      val sextdFromInt32ToWord64: Primitive.Int32.int -> Primitive.Word64.word

      val sextdFromInt64ToInt8: Primitive.Int64.int -> Primitive.Int8.int
      val sextdFromInt64ToInt16: Primitive.Int64.int -> Primitive.Int16.int
      val sextdFromInt64ToInt32: Primitive.Int64.int -> Primitive.Int32.int
      val sextdFromInt64ToInt64: Primitive.Int64.int -> Primitive.Int64.int
      val sextdFromInt64ToWord8: Primitive.Int64.int -> Primitive.Word8.word
      val sextdFromInt64ToWord16: Primitive.Int64.int -> Primitive.Word16.word
      val sextdFromInt64ToWord32: Primitive.Int64.int -> Primitive.Word32.word
      val sextdFromInt64ToWord64: Primitive.Int64.int -> Primitive.Word64.word

      val sextdFromWord8ToInt8: Primitive.Word8.word -> Primitive.Int8.int
      val sextdFromWord8ToInt16: Primitive.Word8.word -> Primitive.Int16.int
      val sextdFromWord8ToInt32: Primitive.Word8.word -> Primitive.Int32.int
      val sextdFromWord8ToInt64: Primitive.Word8.word -> Primitive.Int64.int
      val sextdFromWord8ToWord8: Primitive.Word8.word -> Primitive.Word8.word
      val sextdFromWord8ToWord16: Primitive.Word8.word -> Primitive.Word16.word
      val sextdFromWord8ToWord32: Primitive.Word8.word -> Primitive.Word32.word
      val sextdFromWord8ToWord64: Primitive.Word8.word -> Primitive.Word64.word

      val sextdFromWord16ToInt8: Primitive.Word16.word -> Primitive.Int8.int
      val sextdFromWord16ToInt16: Primitive.Word16.word -> Primitive.Int16.int
      val sextdFromWord16ToInt32: Primitive.Word16.word -> Primitive.Int32.int
      val sextdFromWord16ToInt64: Primitive.Word16.word -> Primitive.Int64.int
      val sextdFromWord16ToWord8: Primitive.Word16.word -> Primitive.Word8.word
      val sextdFromWord16ToWord16: Primitive.Word16.word -> Primitive.Word16.word
      val sextdFromWord16ToWord32: Primitive.Word16.word -> Primitive.Word32.word
      val sextdFromWord16ToWord64: Primitive.Word16.word -> Primitive.Word64.word

      val sextdFromWord32ToInt8: Primitive.Word32.word -> Primitive.Int8.int
      val sextdFromWord32ToInt16: Primitive.Word32.word -> Primitive.Int16.int
      val sextdFromWord32ToInt32: Primitive.Word32.word -> Primitive.Int32.int
      val sextdFromWord32ToInt64: Primitive.Word32.word -> Primitive.Int64.int
      val sextdFromWord32ToWord8: Primitive.Word32.word -> Primitive.Word8.word
      val sextdFromWord32ToWord16: Primitive.Word32.word -> Primitive.Word16.word
      val sextdFromWord32ToWord32: Primitive.Word32.word -> Primitive.Word32.word
      val sextdFromWord32ToWord64: Primitive.Word32.word -> Primitive.Word64.word

      val sextdFromWord64ToInt8: Primitive.Word64.word -> Primitive.Int8.int
      val sextdFromWord64ToInt16: Primitive.Word64.word -> Primitive.Int16.int
      val sextdFromWord64ToInt32: Primitive.Word64.word -> Primitive.Int32.int
      val sextdFromWord64ToInt64: Primitive.Word64.word -> Primitive.Int64.int
      val sextdFromWord64ToWord8: Primitive.Word64.word -> Primitive.Word8.word
      val sextdFromWord64ToWord16: Primitive.Word64.word -> Primitive.Word16.word
      val sextdFromWord64ToWord32: Primitive.Word64.word -> Primitive.Word32.word
      val sextdFromWord64ToWord64: Primitive.Word64.word -> Primitive.Word64.word
   end

structure Primitive = struct

open Primitive

structure IntWordConv : PRIM_INTWORD_CONV =
   struct
      (* identity *)
      val idFromInt8ToInt8 =
         _prim "WordU8_extdToWord8": Int8.int -> Int8.int;
      val idFromInt8ToWord8 =
         _prim "WordU8_extdToWord8": Int8.int -> Word8.word;
      val idFromInt16ToInt16 =
         _prim "WordU16_extdToWord16": Int16.int -> Int16.int;
      val idFromInt16ToWord16 =
         _prim "WordU16_extdToWord16": Int16.int -> Word16.word;
      val idFromInt32ToInt32 =
         _prim "WordU32_extdToWord32": Int32.int -> Int32.int;
      val idFromInt32ToWord32 =
         _prim "WordU32_extdToWord32": Int32.int -> Word32.word;
      val idFromInt64ToInt64 =
         _prim "WordU64_extdToWord64": Int64.int -> Int64.int;
      val idFromInt64ToWord64 =
         _prim "WordU64_extdToWord64": Int64.int -> Word64.word;
      val idFromWord8ToInt8 =
         _prim "WordU8_extdToWord8": Word8.word -> Int8.int;
      val idFromWord8ToWord8 =
         _prim "WordU8_extdToWord8": Word8.word -> Word8.word;
      val idFromWord16ToInt16 =
         _prim "WordU16_extdToWord16": Word16.word -> Int16.int;
      val idFromWord16ToWord16 =
         _prim "WordU16_extdToWord16": Word16.word -> Word16.word;
      val idFromWord32ToInt32 =
         _prim "WordU32_extdToWord32": Word32.word -> Int32.int;
      val idFromWord32ToWord32 =
         _prim "WordU32_extdToWord32": Word32.word -> Word32.word;
      val idFromWord64ToInt64 =
         _prim "WordU64_extdToWord64": Word64.word -> Int64.int;
      val idFromWord64ToWord64 =
         _prim "WordU64_extdToWord64": Word64.word -> Word64.word;

      (* zero-extend or low-bits *)
      val zextdFromInt8ToInt8 =
         _prim "WordU8_extdToWord8": Int8.int -> Int8.int;
      val zextdFromInt8ToInt16 =
         _prim "WordU8_extdToWord16": Int8.int -> Int16.int;
      val zextdFromInt8ToInt32 =
         _prim "WordU8_extdToWord32": Int8.int -> Int32.int;
      val zextdFromInt8ToInt64 =
         _prim "WordU8_extdToWord64": Int8.int -> Int64.int;
      val zextdFromInt8ToWord8 =
         _prim "WordU8_extdToWord8": Int8.int -> Word8.word;
      val zextdFromInt8ToWord16 =
         _prim "WordU8_extdToWord16": Int8.int -> Word16.word;
      val zextdFromInt8ToWord32 =
         _prim "WordU8_extdToWord32": Int8.int -> Word32.word;
      val zextdFromInt8ToWord64 =
         _prim "WordU8_extdToWord64": Int8.int -> Word64.word;

      val zextdFromInt16ToInt8 =
         _prim "WordU16_extdToWord8": Int16.int -> Int8.int;
      val zextdFromInt16ToInt16 =
         _prim "WordU16_extdToWord16": Int16.int -> Int16.int;
      val zextdFromInt16ToInt32 =
         _prim "WordU16_extdToWord32": Int16.int -> Int32.int;
      val zextdFromInt16ToInt64 =
         _prim "WordU16_extdToWord64": Int16.int -> Int64.int;
      val zextdFromInt16ToWord8 =
         _prim "WordU16_extdToWord8": Int16.int -> Word8.word;
      val zextdFromInt16ToWord16 =
         _prim "WordU16_extdToWord16": Int16.int -> Word16.word;
      val zextdFromInt16ToWord32 =
         _prim "WordU16_extdToWord32": Int16.int -> Word32.word;
      val zextdFromInt16ToWord64 =
         _prim "WordU16_extdToWord64": Int16.int -> Word64.word;

      val zextdFromInt32ToInt8 =
         _prim "WordU32_extdToWord8": Int32.int -> Int8.int;
      val zextdFromInt32ToInt16 =
         _prim "WordU32_extdToWord16": Int32.int -> Int16.int;
      val zextdFromInt32ToInt32 =
         _prim "WordU32_extdToWord32": Int32.int -> Int32.int;
      val zextdFromInt32ToInt64 =
         _prim "WordU32_extdToWord64": Int32.int -> Int64.int;
      val zextdFromInt32ToWord8 =
         _prim "WordU32_extdToWord8": Int32.int -> Word8.word;
      val zextdFromInt32ToWord16 =
         _prim "WordU32_extdToWord16": Int32.int -> Word16.word;
      val zextdFromInt32ToWord32 =
         _prim "WordU32_extdToWord32": Int32.int -> Word32.word;
      val zextdFromInt32ToWord64 =
         _prim "WordU32_extdToWord64": Int32.int -> Word64.word;

      val zextdFromInt64ToInt8 =
         _prim "WordU64_extdToWord8": Int64.int -> Int8.int;
      val zextdFromInt64ToInt16 =
         _prim "WordU64_extdToWord16": Int64.int -> Int16.int;
      val zextdFromInt64ToInt32 =
         _prim "WordU64_extdToWord32": Int64.int -> Int32.int;
      val zextdFromInt64ToInt64 =
         _prim "WordU64_extdToWord64": Int64.int -> Int64.int;
      val zextdFromInt64ToWord8 =
         _prim "WordU64_extdToWord8": Int64.int -> Word8.word;
      val zextdFromInt64ToWord16 =
         _prim "WordU64_extdToWord16": Int64.int -> Word16.word;
      val zextdFromInt64ToWord32 =
         _prim "WordU64_extdToWord32": Int64.int -> Word32.word;
      val zextdFromInt64ToWord64 =
         _prim "WordU64_extdToWord64": Int64.int -> Word64.word;

      val zextdFromWord8ToInt8 =
         _prim "WordU8_extdToWord8": Word8.word -> Int8.int;
      val zextdFromWord8ToInt16 =
         _prim "WordU8_extdToWord16": Word8.word -> Int16.int;
      val zextdFromWord8ToInt32 =
         _prim "WordU8_extdToWord32": Word8.word -> Int32.int;
      val zextdFromWord8ToInt64 =
         _prim "WordU8_extdToWord64": Word8.word -> Int64.int;
      val zextdFromWord8ToWord8 =
         _prim "WordU8_extdToWord8": Word8.word -> Word8.word;
      val zextdFromWord8ToWord16 =
         _prim "WordU8_extdToWord16": Word8.word -> Word16.word;
      val zextdFromWord8ToWord32 =
         _prim "WordU8_extdToWord32": Word8.word -> Word32.word;
      val zextdFromWord8ToWord64 =
         _prim "WordU8_extdToWord64": Word8.word -> Word64.word;

      val zextdFromWord16ToInt8 =
         _prim "WordU16_extdToWord8": Word16.word -> Int8.int;
      val zextdFromWord16ToInt16 =
         _prim "WordU16_extdToWord16": Word16.word -> Int16.int;
      val zextdFromWord16ToInt32 =
         _prim "WordU16_extdToWord32": Word16.word -> Int32.int;
      val zextdFromWord16ToInt64 =
         _prim "WordU16_extdToWord64": Word16.word -> Int64.int;
      val zextdFromWord16ToWord8 =
         _prim "WordU16_extdToWord8": Word16.word -> Word8.word;
      val zextdFromWord16ToWord16 =
         _prim "WordU16_extdToWord16": Word16.word -> Word16.word;
      val zextdFromWord16ToWord32 =
         _prim "WordU16_extdToWord32": Word16.word -> Word32.word;
      val zextdFromWord16ToWord64 =
         _prim "WordU16_extdToWord64": Word16.word -> Word64.word;

      val zextdFromWord32ToInt8 =
         _prim "WordU32_extdToWord8": Word32.word -> Int8.int;
      val zextdFromWord32ToInt16 =
         _prim "WordU32_extdToWord16": Word32.word -> Int16.int;
      val zextdFromWord32ToInt32 =
         _prim "WordU32_extdToWord32": Word32.word -> Int32.int;
      val zextdFromWord32ToInt64 =
         _prim "WordU32_extdToWord64": Word32.word -> Int64.int;
      val zextdFromWord32ToWord8 =
         _prim "WordU32_extdToWord8": Word32.word -> Word8.word;
      val zextdFromWord32ToWord16 =
         _prim "WordU32_extdToWord16": Word32.word -> Word16.word;
      val zextdFromWord32ToWord32 =
         _prim "WordU32_extdToWord32": Word32.word -> Word32.word;
      val zextdFromWord32ToWord64 =
         _prim "WordU32_extdToWord64": Word32.word -> Word64.word;

      val zextdFromWord64ToInt8 =
         _prim "WordU64_extdToWord8": Word64.word -> Int8.int;
      val zextdFromWord64ToInt16 =
         _prim "WordU64_extdToWord16": Word64.word -> Int16.int;
      val zextdFromWord64ToInt32 =
         _prim "WordU64_extdToWord32": Word64.word -> Int32.int;
      val zextdFromWord64ToInt64 =
         _prim "WordU64_extdToWord64": Word64.word -> Int64.int;
      val zextdFromWord64ToWord8 =
         _prim "WordU64_extdToWord8": Word64.word -> Word8.word;
      val zextdFromWord64ToWord16 =
         _prim "WordU64_extdToWord16": Word64.word -> Word16.word;
      val zextdFromWord64ToWord32 =
         _prim "WordU64_extdToWord32": Word64.word -> Word32.word;
      val zextdFromWord64ToWord64 =
         _prim "WordU64_extdToWord64": Word64.word -> Word64.word;

      (* sign-extend or low-bits *)
      val sextdFromInt8ToInt8 =
         _prim "WordS8_extdToWord8": Int8.int -> Int8.int;
      val sextdFromInt8ToInt16 =
         _prim "WordS8_extdToWord16": Int8.int -> Int16.int;
      val sextdFromInt8ToInt32 =
         _prim "WordS8_extdToWord32": Int8.int -> Int32.int;
      val sextdFromInt8ToInt64 =
         _prim "WordS8_extdToWord64": Int8.int -> Int64.int;
      val sextdFromInt8ToWord8 =
         _prim "WordS8_extdToWord8": Int8.int -> Word8.word;
      val sextdFromInt8ToWord16 =
         _prim "WordS8_extdToWord16": Int8.int -> Word16.word;
      val sextdFromInt8ToWord32 =
         _prim "WordS8_extdToWord32": Int8.int -> Word32.word;
      val sextdFromInt8ToWord64 =
         _prim "WordS8_extdToWord64": Int8.int -> Word64.word;

      val sextdFromInt16ToInt8 =
         _prim "WordS16_extdToWord8": Int16.int -> Int8.int;
      val sextdFromInt16ToInt16 =
         _prim "WordS16_extdToWord16": Int16.int -> Int16.int;
      val sextdFromInt16ToInt32 =
         _prim "WordS16_extdToWord32": Int16.int -> Int32.int;
      val sextdFromInt16ToInt64 =
         _prim "WordS16_extdToWord64": Int16.int -> Int64.int;
      val sextdFromInt16ToWord8 =
         _prim "WordS16_extdToWord8": Int16.int -> Word8.word;
      val sextdFromInt16ToWord16 =
         _prim "WordS16_extdToWord16": Int16.int -> Word16.word;
      val sextdFromInt16ToWord32 =
         _prim "WordS16_extdToWord32": Int16.int -> Word32.word;
      val sextdFromInt16ToWord64 =
         _prim "WordS16_extdToWord64": Int16.int -> Word64.word;

      val sextdFromInt32ToInt8 =
         _prim "WordS32_extdToWord8": Int32.int -> Int8.int;
      val sextdFromInt32ToInt16 =
         _prim "WordS32_extdToWord16": Int32.int -> Int16.int;
      val sextdFromInt32ToInt32 =
         _prim "WordS32_extdToWord32": Int32.int -> Int32.int;
      val sextdFromInt32ToInt64 =
         _prim "WordS32_extdToWord64": Int32.int -> Int64.int;
      val sextdFromInt32ToWord8 =
         _prim "WordS32_extdToWord8": Int32.int -> Word8.word;
      val sextdFromInt32ToWord16 =
         _prim "WordS32_extdToWord16": Int32.int -> Word16.word;
      val sextdFromInt32ToWord32 =
         _prim "WordS32_extdToWord32": Int32.int -> Word32.word;
      val sextdFromInt32ToWord64 =
         _prim "WordS32_extdToWord64": Int32.int -> Word64.word;

      val sextdFromInt64ToInt8 =
         _prim "WordS64_extdToWord8": Int64.int -> Int8.int;
      val sextdFromInt64ToInt16 =
         _prim "WordS64_extdToWord16": Int64.int -> Int16.int;
      val sextdFromInt64ToInt32 =
         _prim "WordS64_extdToWord32": Int64.int -> Int32.int;
      val sextdFromInt64ToInt64 =
         _prim "WordS64_extdToWord64": Int64.int -> Int64.int;
      val sextdFromInt64ToWord8 =
         _prim "WordS64_extdToWord8": Int64.int -> Word8.word;
      val sextdFromInt64ToWord16 =
         _prim "WordS64_extdToWord16": Int64.int -> Word16.word;
      val sextdFromInt64ToWord32 =
         _prim "WordS64_extdToWord32": Int64.int -> Word32.word;
      val sextdFromInt64ToWord64 =
         _prim "WordS64_extdToWord64": Int64.int -> Word64.word;

      val sextdFromWord8ToInt8 =
         _prim "WordS8_extdToWord8": Word8.word -> Int8.int;
      val sextdFromWord8ToInt16 =
         _prim "WordS8_extdToWord16": Word8.word -> Int16.int;
      val sextdFromWord8ToInt32 =
         _prim "WordS8_extdToWord32": Word8.word -> Int32.int;
      val sextdFromWord8ToInt64 =
         _prim "WordS8_extdToWord64": Word8.word -> Int64.int;
      val sextdFromWord8ToWord8 =
         _prim "WordS8_extdToWord8": Word8.word -> Word8.word;
      val sextdFromWord8ToWord16 =
         _prim "WordS8_extdToWord16": Word8.word -> Word16.word;
      val sextdFromWord8ToWord32 =
         _prim "WordS8_extdToWord32": Word8.word -> Word32.word;
      val sextdFromWord8ToWord64 =
         _prim "WordS8_extdToWord64": Word8.word -> Word64.word;

      val sextdFromWord16ToInt8 =
         _prim "WordS16_extdToWord8": Word16.word -> Int8.int;
      val sextdFromWord16ToInt16 =
         _prim "WordS16_extdToWord16": Word16.word -> Int16.int;
      val sextdFromWord16ToInt32 =
         _prim "WordS16_extdToWord32": Word16.word -> Int32.int;
      val sextdFromWord16ToInt64 =
         _prim "WordS16_extdToWord64": Word16.word -> Int64.int;
      val sextdFromWord16ToWord8 =
         _prim "WordS16_extdToWord8": Word16.word -> Word8.word;
      val sextdFromWord16ToWord16 =
         _prim "WordS16_extdToWord16": Word16.word -> Word16.word;
      val sextdFromWord16ToWord32 =
         _prim "WordS16_extdToWord32": Word16.word -> Word32.word;
      val sextdFromWord16ToWord64 =
         _prim "WordS16_extdToWord64": Word16.word -> Word64.word;

      val sextdFromWord32ToInt8 =
         _prim "WordS32_extdToWord8": Word32.word -> Int8.int;
      val sextdFromWord32ToInt16 =
         _prim "WordS32_extdToWord16": Word32.word -> Int16.int;
      val sextdFromWord32ToInt32 =
         _prim "WordS32_extdToWord32": Word32.word -> Int32.int;
      val sextdFromWord32ToInt64 =
         _prim "WordS32_extdToWord64": Word32.word -> Int64.int;
      val sextdFromWord32ToWord8 =
         _prim "WordS32_extdToWord8": Word32.word -> Word8.word;
      val sextdFromWord32ToWord16 =
         _prim "WordS32_extdToWord16": Word32.word -> Word16.word;
      val sextdFromWord32ToWord32 =
         _prim "WordS32_extdToWord32": Word32.word -> Word32.word;
      val sextdFromWord32ToWord64 =
         _prim "WordS32_extdToWord64": Word32.word -> Word64.word;

      val sextdFromWord64ToInt8 =
         _prim "WordS64_extdToWord8": Word64.word -> Int8.int;
      val sextdFromWord64ToInt16 =
         _prim "WordS64_extdToWord16": Word64.word -> Int16.int;
      val sextdFromWord64ToInt32 =
         _prim "WordS64_extdToWord32": Word64.word -> Int32.int;
      val sextdFromWord64ToInt64 =
         _prim "WordS64_extdToWord64": Word64.word -> Int64.int;
      val sextdFromWord64ToWord8 =
         _prim "WordS64_extdToWord8": Word64.word -> Word8.word;
      val sextdFromWord64ToWord16 =
         _prim "WordS64_extdToWord16": Word64.word -> Word16.word;
      val sextdFromWord64ToWord32 =
         _prim "WordS64_extdToWord32": Word64.word -> Word32.word;
      val sextdFromWord64ToWord64 =
         _prim "WordS64_extdToWord64": Word64.word -> Word64.word;
   end

end
