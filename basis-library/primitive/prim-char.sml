(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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

      val idToWord8 = _prim "WordU8_extdToWord8": char -> Word8.word;
      val idFromWord8 = _prim "WordU8_extdToWord8": Word8.word -> char;
      val idToInt8 = _prim "WordS8_extdToWord8": char -> Int8.int;
      val idFromInt8 = _prim "WordS8_extdToWord8": Int8.int -> char;
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

      val idToWord16 = _prim "WordU16_extdToWord16": char -> Word16.word;
      val idFromWord16 = _prim "WordU16_extdToWord16": Word16.word -> char;
      val idToInt16 = _prim "WordS16_extdToWord16": char -> Int16.int;
      val idFromInt16 = _prim "WordS16_extdToWord16": Int16.int -> char;
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

      val idToWord32 = _prim "WordU32_extdToWord32": char -> Word32.word;
      val idFromWord32 = _prim "WordU32_extdToWord32": Word32.word -> char;
      val idToInt32 = _prim "WordS32_extdToWord32": char -> Int32.int;
      val idFromInt32 = _prim "WordS32_extdToWord32": Int32.int -> char;
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
