(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
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

      val wordSize: Primitive.Int32.int

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

      (* Lowbits or sign extend. *)
      val fromInt8Unsafe: Primitive.Int8.int -> word
      val fromInt16Unsafe: Primitive.Int16.int -> word
      val fromInt32Unsafe: Primitive.Int32.int -> word
      val fromInt64Unsafe: Primitive.Int64.int -> word

(*
      (* Lowbits or zero extend. *)
      val fromInt8ZUnsafe: Primitive.Int8.int -> word
      val fromInt16ZUnsafe: Primitive.Int16.int -> word
      val fromInt32ZUnsafe: Primitive.Int32.int -> word
      val fromInt64ZUnsafe: Primitive.Int64.int -> word
*)

      (* Lowbits or zero extend. *)
      val fromWord8Unsafe: Primitive.Word8.word -> word
      val fromWord16Unsafe: Primitive.Word16.word -> word
      val fromWord32Unsafe: Primitive.Word32.word -> word
      val fromWord64Unsafe: Primitive.Word64.word -> word

      (* Lowbits or sign extend. *)
      val fromWord8XUnsafe: Primitive.Word8.word -> word
      val fromWord16XUnsafe: Primitive.Word16.word -> word
      val fromWord32XUnsafe: Primitive.Word32.word -> word
      val fromWord64XUnsafe: Primitive.Word64.word -> word

      (* Lowbits or zero extend. *)
      val toInt8Unsafe: word -> Primitive.Int8.int
      val toInt16Unsafe: word -> Primitive.Int16.int
      val toInt32Unsafe: word -> Primitive.Int32.int
      val toInt64Unsafe: word -> Primitive.Int64.int

      (* Lowbits or sign extend. *)
      val toInt8XUnsafe: word -> Primitive.Int8.int
      val toInt16XUnsafe: word -> Primitive.Int16.int
      val toInt32XUnsafe: word -> Primitive.Int32.int
      val toInt64XUnsafe: word -> Primitive.Int64.int

      (* Lowbits or zero extend. *)
      val toWord8Unsafe: word -> Primitive.Word8.word
      val toWord16Unsafe: word -> Primitive.Word16.word
      val toWord32Unsafe: word -> Primitive.Word32.word
      val toWord64Unsafe: word -> Primitive.Word64.word

      (* Lowbits or sign extend. *)
      val toWord8XUnsafe: word -> Primitive.Word8.word
      val toWord16XUnsafe: word -> Primitive.Word16.word
      val toWord32XUnsafe: word -> Primitive.Word32.word
      val toWord64XUnsafe: word -> Primitive.Word64.word
   end

structure Primitive = struct

open Primitive

structure Word1 =
   struct
      open Word1
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_toWord1": big -> word;
      val toBig = _prim "WordU1_toWord8": word -> big;
      val wordSize: Int32.int = 1
   end
structure Word2 =
   struct
      open Word2
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_toWord2": big -> word;
      val toBig = _prim "WordU2_toWord8": word -> big;
      val wordSize: Int32.int = 2
   end
structure Word3 =
   struct
      open Word3
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_toWord3": big -> word;
      val toBig = _prim "WordU3_toWord8": word -> big;
      val wordSize: Int32.int = 3
   end
structure Word4 =
   struct
      open Word4
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_toWord4": big -> word;
      val toBig = _prim "WordU4_toWord8": word -> big;
      val wordSize: Int32.int = 4
   end
structure Word5 =
   struct
      open Word5
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_toWord5": big -> word;
      val toBig = _prim "WordU5_toWord8": word -> big;
      val wordSize: Int32.int = 5
   end
structure Word6 =
   struct
      open Word6
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_toWord6": big -> word;
      val toBig = _prim "WordU6_toWord8": word -> big;
      val wordSize: Int32.int = 6
   end
structure Word7 =
   struct
      open Word7
      type big = Word8.word
      val fromBigUnsafe = _prim "WordU8_toWord7": big -> word;
      val toBig = _prim "WordU7_toWord8": word -> big;
      val wordSize: Int32.int = 7
   end
structure Word8 =
   struct
      open Word8
         
      val wordSize: Int32.int = 8

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

      val fromInt8Unsafe = _prim "WordS8_toWord8": Int8.int -> word;
      val fromInt16Unsafe = _prim "WordS16_toWord8": Int16.int -> word;
      val fromInt32Unsafe = _prim "WordS32_toWord8": Int32.int -> word;
      val fromInt64Unsafe = _prim "WordS64_toWord8": Int64.int -> word;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord8": Int8.int -> word;
      val fromInt16ZUnsafe = _prim "WordU16_toWord8": Int16.int -> word;
      val fromInt32ZUnsafe = _prim "WordU32_toWord8": Int32.int -> word;
      val fromInt64ZUnsafe = _prim "WordU64_toWord8": Int64.int -> word;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord8": Word8.word -> word;
      val fromWord16Unsafe = _prim "WordU16_toWord8": Word16.word -> word;
      val fromWord32Unsafe = _prim "WordU32_toWord8": Word32.word -> word;
      val fromWord64Unsafe = _prim "WordU64_toWord8": Word64.word -> word;

      val fromWord8XUnsafe = _prim "WordS8_toWord8": Word8.word -> word;
      val fromWord16XUnsafe = _prim "WordS16_toWord8": Word16.word -> word;
      val fromWord32XUnsafe = _prim "WordS32_toWord8": Word32.word -> word;
      val fromWord64XUnsafe = _prim "WordS64_toWord8": Word64.word -> word;

      val toInt8Unsafe = _prim "WordU8_toWord8": word -> Int8.int;
      val toInt16Unsafe = _prim "WordU8_toWord16": word -> Int16.int;
      val toInt32Unsafe = _prim "WordU8_toWord32": word -> Int32.int;
      val toInt64Unsafe = _prim "WordU8_toWord64": word -> Int64.int;

      val toInt8XUnsafe = _prim "WordS8_toWord8": word -> Int8.int;
      val toInt16XUnsafe = _prim "WordS8_toWord16": word -> Int16.int;
      val toInt32XUnsafe = _prim "WordS8_toWord32": word -> Int32.int;
      val toInt64XUnsafe = _prim "WordS8_toWord64": word -> Int64.int;

      val toWord8Unsafe = _prim "WordU8_toWord8": word -> Word8.word;
      val toWord16Unsafe = _prim "WordU8_toWord16": word -> Word16.word;
      val toWord32Unsafe = _prim "WordU8_toWord32": word -> Word32.word;
      val toWord64Unsafe = _prim "WordU8_toWord64": word -> Word64.word;

      val toWord8XUnsafe = _prim "WordS8_toWord8": word -> Word8.word;
      val toWord16XUnsafe = _prim "WordS8_toWord16": word -> Word16.word;
      val toWord32XUnsafe = _prim "WordS8_toWord32": word -> Word32.word;
      val toWord64XUnsafe = _prim "WordS8_toWord64": word -> Word64.word;
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
      val fromBigUnsafe = _prim "WordU16_toWord9": big -> word;
      val toBig = _prim "WordU9_toWord16": word -> big;
      val wordSize: Int32.int = 9
   end
structure Word10 =
   struct
      open Word10
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_toWord10": big -> word;
      val toBig = _prim "WordU10_toWord16": word -> big;
      val wordSize: Int32.int = 10
   end
structure Word11 =
   struct
      open Word11
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_toWord11": big -> word;
      val toBig = _prim "WordU11_toWord16": word -> big;
      val wordSize: Int32.int = 11
   end
structure Word12 =
   struct
      open Word12
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_toWord12": big -> word;
      val toBig = _prim "WordU12_toWord16": word -> big;
      val wordSize: Int32.int = 12
   end
structure Word13 =
   struct
      open Word13
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_toWord13": big -> word;
      val toBig = _prim "WordU13_toWord16": word -> big;
      val wordSize: Int32.int = 13
   end
structure Word14 =
   struct
      open Word14
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_toWord14": big -> word;
      val toBig = _prim "WordU14_toWord16": word -> big;
      val wordSize: Int32.int = 14
   end
structure Word15 =
   struct
      open Word15
      type big = Word16.word
      val fromBigUnsafe = _prim "WordU16_toWord15": big -> word;
      val toBig = _prim "WordU15_toWord16": word -> big;
      val wordSize: Int32.int = 15
   end
structure Word16 =
   struct
      open Word16
         
      val wordSize: Int32.int = 16

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

      val fromInt8Unsafe = _prim "WordS8_toWord16": Int8.int -> word;
      val fromInt16Unsafe = _prim "WordS16_toWord16": Int16.int -> word;
      val fromInt32Unsafe = _prim "WordS32_toWord16": Int32.int -> word;
      val fromInt64Unsafe = _prim "WordS64_toWord16": Int64.int -> word;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord16": Int8.int -> word;
      val fromInt16ZUnsafe = _prim "WordU16_toWord16": Int16.int -> word;
      val fromInt32ZUnsafe = _prim "WordU32_toWord16": Int32.int -> word;
      val fromInt64ZUnsafe = _prim "WordU64_toWord16": Int64.int -> word;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord16": Word8.word -> word;
      val fromWord16Unsafe = _prim "WordU16_toWord16": Word16.word -> word;
      val fromWord32Unsafe = _prim "WordU32_toWord16": Word32.word -> word;
      val fromWord64Unsafe = _prim "WordU64_toWord16": Word64.word -> word;

      val fromWord8XUnsafe = _prim "WordS8_toWord16": Word8.word -> word;
      val fromWord16XUnsafe = _prim "WordS16_toWord16": Word16.word -> word;
      val fromWord32XUnsafe = _prim "WordS32_toWord16": Word32.word -> word;
      val fromWord64XUnsafe = _prim "WordS64_toWord16": Word64.word -> word;

      val toInt8Unsafe = _prim "WordU16_toWord8": word -> Int8.int;
      val toInt16Unsafe = _prim "WordU16_toWord16": word -> Int16.int;
      val toInt32Unsafe = _prim "WordU16_toWord32": word -> Int32.int;
      val toInt64Unsafe = _prim "WordU16_toWord64": word -> Int64.int;

      val toInt8XUnsafe = _prim "WordS16_toWord8": word -> Int8.int;
      val toInt16XUnsafe = _prim "WordS16_toWord16": word -> Int16.int;
      val toInt32XUnsafe = _prim "WordS16_toWord32": word -> Int32.int;
      val toInt64XUnsafe = _prim "WordS16_toWord64": word -> Int64.int;

      val toWord8Unsafe = _prim "WordU16_toWord8": word -> Word8.word;
      val toWord16Unsafe = _prim "WordU16_toWord16": word -> Word16.word;
      val toWord32Unsafe = _prim "WordU16_toWord32": word -> Word32.word;
      val toWord64Unsafe = _prim "WordU16_toWord64": word -> Word64.word;

      val toWord8XUnsafe = _prim "WordS16_toWord8": word -> Word8.word;
      val toWord16XUnsafe = _prim "WordS16_toWord16": word -> Word16.word;
      val toWord32XUnsafe = _prim "WordS16_toWord32": word -> Word32.word;
      val toWord64XUnsafe = _prim "WordS16_toWord64": word -> Word64.word;
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
      val fromBigUnsafe = _prim "WordU32_toWord17": big -> word;
      val toBig = _prim "WordU17_toWord32": word -> big;
      val wordSize: Int32.int = 17
   end
structure Word18 =
   struct
      open Word18
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord18": big -> word;
      val toBig = _prim "WordU18_toWord32": word -> big;
      val wordSize: Int32.int = 18
   end
structure Word19 =
   struct
      open Word19
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord19": big -> word;
      val toBig = _prim "WordU19_toWord32": word -> big;
      val wordSize: Int32.int = 19
   end
structure Word20 =
   struct
      open Word20
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord20": big -> word;
      val toBig = _prim "WordU20_toWord32": word -> big;
      val wordSize: Int32.int = 20
   end
structure Word21 =
   struct
      open Word21
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord21": big -> word;
      val toBig = _prim "WordU21_toWord32": word -> big;
      val wordSize: Int32.int = 21
   end
structure Word22 =
   struct
      open Word22
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord22": big -> word;
      val toBig = _prim "WordU22_toWord32": word -> big;
      val wordSize: Int32.int = 22
   end
structure Word23 =
   struct
      open Word23
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord23": big -> word;
      val toBig = _prim "WordU23_toWord32": word -> big;
      val wordSize: Int32.int = 23
   end
structure Word24 =
   struct
      open Word24
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord24": big -> word;
      val toBig = _prim "WordU24_toWord32": word -> big;
      val wordSize: Int32.int = 24
   end
structure Word25 =
   struct
      open Word25
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord25": big -> word;
      val toBig = _prim "WordU25_toWord32": word -> big;
      val wordSize: Int32.int = 25
   end
structure Word26 =
   struct
      open Word26
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord26": big -> word;
      val toBig = _prim "WordU26_toWord32": word -> big;
      val wordSize: Int32.int = 26
   end
structure Word27 =
   struct
      open Word27
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord27": big -> word;
      val toBig = _prim "WordU27_toWord32": word -> big;
      val wordSize: Int32.int = 27
   end
structure Word28 =
   struct
      open Word28
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord28": big -> word;
      val toBig = _prim "WordU28_toWord32": word -> big;
      val wordSize: Int32.int = 28
   end
structure Word29 =
   struct
      open Word29
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord29": big -> word;
      val toBig = _prim "WordU29_toWord32": word -> big;
      val wordSize: Int32.int = 29
   end
structure Word30 =
   struct
      open Word30
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord30": big -> word;
      val toBig = _prim "WordU30_toWord32": word -> big;
      val wordSize: Int32.int = 30
   end
structure Word31 =
   struct
      open Word31
      type big = Word32.word
      val fromBigUnsafe = _prim "WordU32_toWord31": big -> word;
      val toBig = _prim "WordU31_toWord32": word -> big;
      val wordSize: Int32.int = 31
   end
structure Word32 =
   struct
      open Word32
         
      val wordSize: Int32.int = 32

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

      val fromInt8Unsafe = _prim "WordS8_toWord32": Int8.int -> word;
      val fromInt16Unsafe = _prim "WordS16_toWord32": Int16.int -> word;
      val fromInt32Unsafe = _prim "WordS32_toWord32": Int32.int -> word;
      val fromInt64Unsafe = _prim "WordS64_toWord32": Int64.int -> word;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord32": Int8.int -> word;
      val fromInt16ZUnsafe = _prim "WordU16_toWord32": Int16.int -> word;
      val fromInt32ZUnsafe = _prim "WordU32_toWord32": Int32.int -> word;
      val fromInt64ZUnsafe = _prim "WordU64_toWord32": Int64.int -> word;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord32": Word8.word -> word;
      val fromWord16Unsafe = _prim "WordU16_toWord32": Word16.word -> word;
      val fromWord32Unsafe = _prim "WordU32_toWord32": Word32.word -> word;
      val fromWord64Unsafe = _prim "WordU64_toWord32": Word64.word -> word;

      val fromWord8XUnsafe = _prim "WordS8_toWord32": Word8.word -> word;
      val fromWord16XUnsafe = _prim "WordS16_toWord32": Word16.word -> word;
      val fromWord32XUnsafe = _prim "WordS32_toWord32": Word32.word -> word;
      val fromWord64XUnsafe = _prim "WordS64_toWord32": Word64.word -> word;

      val toInt8Unsafe = _prim "WordU32_toWord8": word -> Int8.int;
      val toInt16Unsafe = _prim "WordU32_toWord16": word -> Int16.int;
      val toInt32Unsafe = _prim "WordU32_toWord32": word -> Int32.int;
      val toInt64Unsafe = _prim "WordU32_toWord64": word -> Int64.int;

      val toInt8XUnsafe = _prim "WordS32_toWord8": word -> Int8.int;
      val toInt16XUnsafe = _prim "WordS32_toWord16": word -> Int16.int;
      val toInt32XUnsafe = _prim "WordS32_toWord32": word -> Int32.int;
      val toInt64XUnsafe = _prim "WordS32_toWord64": word -> Int64.int;

      val toWord8Unsafe = _prim "WordU32_toWord8": word -> Word8.word;
      val toWord16Unsafe = _prim "WordU32_toWord16": word -> Word16.word;
      val toWord32Unsafe = _prim "WordU32_toWord32": word -> Word32.word;
      val toWord64Unsafe = _prim "WordU32_toWord64": word -> Word64.word;

      val toWord8XUnsafe = _prim "WordS32_toWord8": word -> Word8.word;
      val toWord16XUnsafe = _prim "WordS32_toWord16": word -> Word16.word;
      val toWord32XUnsafe = _prim "WordS32_toWord32": word -> Word32.word;
      val toWord64XUnsafe = _prim "WordS32_toWord64": word -> Word64.word;
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
         
      val wordSize: Int32.int = 64

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

      val fromInt8Unsafe = _prim "WordS8_toWord64": Int8.int -> word;
      val fromInt16Unsafe = _prim "WordS16_toWord64": Int16.int -> word;
      val fromInt32Unsafe = _prim "WordS32_toWord64": Int32.int -> word;
      val fromInt64Unsafe = _prim "WordS64_toWord64": Int64.int -> word;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord64": Int8.int -> word;
      val fromInt16ZUnsafe = _prim "WordU16_toWord64": Int16.int -> word;
      val fromInt32ZUnsafe = _prim "WordU32_toWord64": Int32.int -> word;
      val fromInt64ZUnsafe = _prim "WordU64_toWord64": Int64.int -> word;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord64": Word8.word -> word;
      val fromWord16Unsafe = _prim "WordU16_toWord64": Word16.word -> word;
      val fromWord32Unsafe = _prim "WordU32_toWord64": Word32.word -> word;
      val fromWord64Unsafe = _prim "WordU64_toWord64": Word64.word -> word;

      val fromWord8XUnsafe = _prim "WordS8_toWord64": Word8.word -> word;
      val fromWord16XUnsafe = _prim "WordS16_toWord64": Word16.word -> word;
      val fromWord32XUnsafe = _prim "WordS32_toWord64": Word32.word -> word;
      val fromWord64XUnsafe = _prim "WordS64_toWord64": Word64.word -> word;

      val toInt8Unsafe = _prim "WordU64_toWord8": word -> Int8.int;
      val toInt16Unsafe = _prim "WordU64_toWord16": word -> Int16.int;
      val toInt32Unsafe = _prim "WordU64_toWord32": word -> Int32.int;
      val toInt64Unsafe = _prim "WordU64_toWord64": word -> Int64.int;

      val toInt8XUnsafe = _prim "WordS64_toWord8": word -> Int8.int;
      val toInt16XUnsafe = _prim "WordS64_toWord16": word -> Int16.int;
      val toInt32XUnsafe = _prim "WordS64_toWord32": word -> Int32.int;
      val toInt64XUnsafe = _prim "WordS64_toWord64": word -> Int64.int;

      val toWord8Unsafe = _prim "WordU64_toWord8": word -> Word8.word;
      val toWord16Unsafe = _prim "WordU64_toWord16": word -> Word16.word;
      val toWord32Unsafe = _prim "WordU64_toWord32": word -> Word32.word;
      val toWord64Unsafe = _prim "WordU64_toWord64": word -> Word64.word;

      val toWord8XUnsafe = _prim "WordS64_toWord8": word -> Word8.word;
      val toWord16XUnsafe = _prim "WordS64_toWord16": word -> Word16.word;
      val toWord32XUnsafe = _prim "WordS64_toWord32": word -> Word32.word;
      val toWord64XUnsafe = _prim "WordS64_toWord64": word -> Word64.word;
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
