(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

signature PRIM_WORD =
   sig
      eqtype word
      type t = word

      val sizeInBits: Primitive.Int32.int
      val sizeInBitsWord: Primitive.Word32.word

      val + : word * word -> word
      val andb : word * word -> word
      val <<? : word * Primitive.Word32.word -> word
      val * : word * word -> word
      val ~ : word -> word
      val notb : word -> word
      val orb : word * word -> word
      val quotUnsafe : word * word -> word
      val remUnsafe: word * word -> word
      val rolUnsafe: word * Primitive.Word32.word -> word
      val rorUnsafe: word * Primitive.Word32.word -> word
      val ~>>? : word * Primitive.Word32.word -> word
      val >>? : word * Primitive.Word32.word -> word
      val - : word * word -> word
      val xorb: word * word -> word

      val < : word * word -> bool
      val <= : word * word -> bool
      val > : word * word -> bool
      val >= : word * word -> bool
      val compare: word * word -> Primitive.Order.order
      val min: word * word -> word
      val max: word * word -> word
   end

structure Primitive = struct

open Primitive

structure Word1 =
   struct
      open Word1
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_extdToWord1": big -> word;
      val toBig = _prim "WordU1_extdToWord8": word -> big;
      val sizeInBits: Int32.int = 1
   end
structure Word2 =
   struct
      open Word2
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_extdToWord2": big -> word;
      val toBig = _prim "WordU2_extdToWord8": word -> big;
      val sizeInBits: Int32.int = 2
   end
structure Word3 =
   struct
      open Word3
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_extdToWord3": big -> word;
      val toBig = _prim "WordU3_extdToWord8": word -> big;
      val sizeInBits: Int32.int = 3
   end
structure Word4 =
   struct
      open Word4
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_extdToWord4": big -> word;
      val toBig = _prim "WordU4_extdToWord8": word -> big;
      val sizeInBits: Int32.int = 4
   end
structure Word5 =
   struct
      open Word5
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_extdToWord5": big -> word;
      val toBig = _prim "WordU5_extdToWord8": word -> big;
      val sizeInBits: Int32.int = 5
   end
structure Word6 =
   struct
      open Word6
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_extdToWord6": big -> word;
      val toBig = _prim "WordU6_extdToWord8": word -> big;
      val sizeInBits: Int32.int = 6
   end
structure Word7 =
   struct
      open Word7
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_extdToWord7": big -> word;
      val toBig = _prim "WordU7_extdToWord8": word -> big;
      val sizeInBits: Int32.int = 7
   end
structure Word8 =
   struct
      open Word8

      val sizeInBits: Int32.int = 8
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits

      val + = _prim "Word8_add": word * word -> word;
      val andb = _prim "Word8_andb": word * word -> word;
      val <<? = _prim "Word8_lshift": word * Word32.word -> word;
      val * = _prim "WordU8_mul": word * word -> word;
      val ~ = _prim "Word8_neg": word -> word;
      val notb = _prim "Word8_notb": word -> word;
      val orb = _prim "Word8_orb": word * word -> word;
      val quotUnsafe = _prim "WordU8_quot": word * word -> word;
      val remUnsafe = _prim "WordU8_rem": word * word -> word;
      val rolUnsafe = _prim "Word8_rol": word * Word32.word -> word;
      val rorUnsafe = _prim "Word8_ror": word * Word32.word -> word;
      val ~>>? = _prim "WordS8_rshift": word * Word32.word -> word;
      val >>? = _prim "WordU8_rshift": word * Word32.word -> word;
      val - = _prim "Word8_sub": word * word -> word;
      val xorb = _prim "Word8_xorb": word * word -> word;

      val < = _prim "WordU8_lt": word * word -> bool;
   end
structure Word8 : PRIM_WORD =
   struct
      open Word8
      local
         structure S = IntegralComparisons(Word8)
      in
         open S
      end
   end
structure Word9 =
   struct
      open Word9
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_extdToWord9": big -> word;
      val toBig = _prim "WordU9_extdToWord16": word -> big;
      val sizeInBits: Int32.int = 9
   end
structure Word10 =
   struct
      open Word10
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_extdToWord10": big -> word;
      val toBig = _prim "WordU10_extdToWord16": word -> big;
      val sizeInBits: Int32.int = 10
   end
structure Word11 =
   struct
      open Word11
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_extdToWord11": big -> word;
      val toBig = _prim "WordU11_extdToWord16": word -> big;
      val sizeInBits: Int32.int = 11
   end
structure Word12 =
   struct
      open Word12
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_extdToWord12": big -> word;
      val toBig = _prim "WordU12_extdToWord16": word -> big;
      val sizeInBits: Int32.int = 12
   end
structure Word13 =
   struct
      open Word13
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_extdToWord13": big -> word;
      val toBig = _prim "WordU13_extdToWord16": word -> big;
      val sizeInBits: Int32.int = 13
   end
structure Word14 =
   struct
      open Word14
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_extdToWord14": big -> word;
      val toBig = _prim "WordU14_extdToWord16": word -> big;
      val sizeInBits: Int32.int = 14
   end
structure Word15 =
   struct
      open Word15
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_extdToWord15": big -> word;
      val toBig = _prim "WordU15_extdToWord16": word -> big;
      val sizeInBits: Int32.int = 15
   end
structure Word16 =
   struct
      open Word16

      val sizeInBits: Int32.int = 16
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits

      val + = _prim "Word16_add": word * word -> word;
      val andb = _prim "Word16_andb": word * word -> word;
      val <<? = _prim "Word16_lshift": word * Word32.word -> word;
      val * = _prim "WordU16_mul": word * word -> word;
      val ~ = _prim "Word16_neg": word -> word;
      val notb = _prim "Word16_notb": word -> word;
      val orb = _prim "Word16_orb": word * word -> word;
      val quotUnsafe = _prim "WordU16_quot": word * word -> word;
      val remUnsafe = _prim "WordU16_rem": word * word -> word;
      val rolUnsafe = _prim "Word16_rol": word * Word32.word -> word;
      val rorUnsafe = _prim "Word16_ror": word * Word32.word -> word;
      val ~>>? = _prim "WordS16_rshift": word * Word32.word -> word;
      val >>? = _prim "WordU16_rshift": word * Word32.word -> word;
      val - = _prim "Word16_sub": word * word -> word;
      val xorb = _prim "Word16_xorb": word * word -> word;

      val < = _prim "WordU16_lt": word * word -> bool;
   end
structure Word16 : PRIM_WORD =
   struct
      open Word16
      local
         structure S = IntegralComparisons(Word16)
      in
         open S
      end
   end
structure Word17 =
   struct
      open Word17
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord17": big -> word;
      val toBig = _prim "WordU17_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 17
   end
structure Word18 =
   struct
      open Word18
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord18": big -> word;
      val toBig = _prim "WordU18_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 18
   end
structure Word19 =
   struct
      open Word19
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord19": big -> word;
      val toBig = _prim "WordU19_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 19
   end
structure Word20 =
   struct
      open Word20
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord20": big -> word;
      val toBig = _prim "WordU20_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 20
   end
structure Word21 =
   struct
      open Word21
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord21": big -> word;
      val toBig = _prim "WordU21_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 21
   end
structure Word22 =
   struct
      open Word22
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord22": big -> word;
      val toBig = _prim "WordU22_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 22
   end
structure Word23 =
   struct
      open Word23
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord23": big -> word;
      val toBig = _prim "WordU23_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 23
   end
structure Word24 =
   struct
      open Word24
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord24": big -> word;
      val toBig = _prim "WordU24_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 24
   end
structure Word25 =
   struct
      open Word25
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord25": big -> word;
      val toBig = _prim "WordU25_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 25
   end
structure Word26 =
   struct
      open Word26
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord26": big -> word;
      val toBig = _prim "WordU26_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 26
   end
structure Word27 =
   struct
      open Word27
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord27": big -> word;
      val toBig = _prim "WordU27_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 27
   end
structure Word28 =
   struct
      open Word28
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord28": big -> word;
      val toBig = _prim "WordU28_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 28
   end
structure Word29 =
   struct
      open Word29
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord29": big -> word;
      val toBig = _prim "WordU29_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 29
   end
structure Word30 =
   struct
      open Word30
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord30": big -> word;
      val toBig = _prim "WordU30_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 30
   end
structure Word31 =
   struct
      open Word31
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_extdToWord31": big -> word;
      val toBig = _prim "WordU31_extdToWord32": word -> big;
      val sizeInBits: Int32.int = 31
   end
structure Word32 =
   struct
      open Word32

      val sizeInBits: Int32.int = 32
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits

      val + = _prim "Word32_add": word * word -> word;
      val andb = _prim "Word32_andb": word * word -> word;
      val <<? = _prim "Word32_lshift": word * Word32.word -> word;
      val * = _prim "WordU32_mul": word * word -> word;
      val ~ = _prim "Word32_neg": word -> word;
      val notb = _prim "Word32_notb": word -> word;
      val orb = _prim "Word32_orb": word * word -> word;
      val quotUnsafe = _prim "WordU32_quot": word * word -> word;
      val remUnsafe = _prim "WordU32_rem": word * word -> word;
      val rolUnsafe = _prim "Word32_rol": word * Word32.word -> word;
      val rorUnsafe = _prim "Word32_ror": word * Word32.word -> word;
      val ~>>? = _prim "WordS32_rshift": word * Word32.word -> word;
      val >>? = _prim "WordU32_rshift": word * Word32.word -> word;
      val - = _prim "Word32_sub": word * word -> word;
      val xorb = _prim "Word32_xorb": word * word -> word;

      val < = _prim "WordU32_lt": word * word -> bool;
   end
structure Word32 : PRIM_WORD =
   struct
      open Word32
      local
         structure S = IntegralComparisons(Word32)
      in
         open S
      end
   end
structure Word64 =
   struct
      open Word64

      val sizeInBits: Int32.int = 64
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits

      val + = _prim "Word64_add": word * word -> word;
      val andb = _prim "Word64_andb": word * word -> word;
      val <<? = _prim "Word64_lshift": word * Word32.word -> word;
      val * = _prim "WordU64_mul": word * word -> word;
      val ~ = _prim "Word64_neg": word -> word;
      val notb = _prim "Word64_notb": word -> word;
      val orb = _prim "Word64_orb": word * word -> word;
      val quotUnsafe = _prim "WordU64_quot": word * word -> word;
      val remUnsafe = _prim "WordU64_rem": word * word -> word;
      val rolUnsafe = _prim "Word64_rol": word * Word32.word -> word;
      val rorUnsafe = _prim "Word64_ror": word * Word32.word -> word;
      val ~>>? = _prim "WordS64_rshift": word * Word32.word -> word;
      val >>? = _prim "WordU64_rshift": word * Word32.word -> word;
      val - = _prim "Word64_sub": word * word -> word;
      val xorb = _prim "Word64_xorb": word * word -> word;

      val < = _prim "WordU64_lt": word * word -> bool;
   end
structure Word64 : PRIM_WORD =
   struct
      open Word64
      local
         structure S = IntegralComparisons(Word64)
      in
         open S
      end
   end

end
