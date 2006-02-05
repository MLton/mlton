(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure Char8 =
   struct
      open Char8
         
      val < = _prim "WordU8_lt": char * char -> bool;

      val fromInt8 = _prim "WordS8_toWord8": Int8.int -> char;
      val fromInt16 = _prim "WordS16_toWord8": Int16.int -> char;
      val fromInt32 = _prim "WordS32_toWord8": Int32.int -> char;
      val fromInt64 = _prim "WordS64_toWord8": Int64.int -> char;

      val fromWord8 = _prim "WordU8_toWord8": Word8.word -> char;
      val fromWord16 = _prim "WordU16_toWord8": Word16.word -> char;
      val fromWord32 = _prim "WordU32_toWord8": Word32.word -> char;
      val fromWord64 = _prim "WordU64_toWord8": Word64.word -> char;

      val toInt8 = _prim "WordS8_toWord8": char -> Int8.int;
      val toInt16 = _prim "WordS8_toWord16": char -> Int16.int;
      val toInt32 = _prim "WordS8_toWord32": char -> Int32.int;
      val toInt64 = _prim "WordS8_toWord64": char -> Int64.int;

      val toWord8 = _prim "WordU8_toWord8": char -> Word8.word;
      val toWord16 = _prim "WordU8_toWord16": char -> Word16.word;
      val toWord32 = _prim "WordU8_toWord32": char -> Word32.word;
      val toWord64 = _prim "WordU8_toWord64": char -> Word64.word;
   end
structure Char8 = 
   struct
      open Char8
      local
         structure S = IntegralComparisons(Char8)
      in
         open S
      end
   end

structure Char16 =
   struct
      open Char16
         
      val < = _prim "WordU16_lt": char * char -> bool;

      val fromInt8 = _prim "WordS8_toWord16": Int8.int -> char;
      val fromInt16 = _prim "WordS16_toWord16": Int16.int -> char;
      val fromInt32 = _prim "WordS32_toWord16": Int32.int -> char;
      val fromInt64 = _prim "WordS64_toWord16": Int64.int -> char;

      val fromWord8 = _prim "WordU8_toWord16": Word8.word -> char;
      val fromWord16 = _prim "WordU16_toWord16": Word16.word -> char;
      val fromWord32 = _prim "WordU32_toWord16": Word32.word -> char;
      val fromWord64 = _prim "WordU64_toWord16": Word64.word -> char;

      val toInt8 = _prim "WordS16_toWord8": char -> Int8.int;
      val toInt16 = _prim "WordS16_toWord16": char -> Int16.int;
      val toInt32 = _prim "WordS16_toWord32": char -> Int32.int;
      val toInt64 = _prim "WordS16_toWord64": char -> Int64.int;

      val toWord8 = _prim "WordU16_toWord8": char -> Word8.word;
      val toWord16 = _prim "WordU16_toWord16": char -> Word16.word;
      val toWord32 = _prim "WordU16_toWord32": char -> Word32.word;
      val toWord64 = _prim "WordU16_toWord64": char -> Word64.word;
   end
structure Char16 = 
   struct
      open Char16
      local
         structure S = IntegralComparisons(Char16)
      in
         open S
      end
   end

structure Char32 =
   struct
      open Char32
         
      val < = _prim "WordU32_lt": char * char -> bool;

      val fromInt8 = _prim "WordS8_toWord32": Int8.int -> char;
      val fromInt16 = _prim "WordS16_toWord32": Int16.int -> char;
      val fromInt32 = _prim "WordS32_toWord32": Int32.int -> char;
      val fromInt64 = _prim "WordS64_toWord32": Int64.int -> char;

      val fromWord8 = _prim "WordU8_toWord32": Word8.word -> char;
      val fromWord16 = _prim "WordU16_toWord32": Word16.word -> char;
      val fromWord32 = _prim "WordU32_toWord32": Word32.word -> char;
      val fromWord64 = _prim "WordU64_toWord32": Word64.word -> char;

      val toInt8 = _prim "WordS32_toWord8": char -> Int8.int;
      val toInt16 = _prim "WordS32_toWord16": char -> Int16.int;
      val toInt32 = _prim "WordS32_toWord32": char -> Int32.int;
      val toInt64 = _prim "WordS32_toWord64": char -> Int64.int;

      val toWord8 = _prim "WordU32_toWord8": char -> Word8.word;
      val toWord16 = _prim "WordU32_toWord16": char -> Word16.word;
      val toWord32 = _prim "WordU32_toWord32": char -> Word32.word;
      val toWord64 = _prim "WordU32_toWord64": char -> Word64.word;
   end
structure Char32 = 
   struct
      open Char32
      local
         structure S = IntegralComparisons(Char32)
      in
         open S
      end
   end

end
