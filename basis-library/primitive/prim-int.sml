(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

signature PRIM_INTEGER =
   sig
      eqtype int
      type t = int

      val precision' : Primitive.Int32.int
      val maxInt': int
      val minInt': int
       
      val +? : int * int -> int
      val + : int * int -> int
      val *? : int * int -> int
      val * : int * int -> int
      val ~? : int -> int
      val ~ : int -> int
      val quotUnsafe: int * int -> int
      val -? : int * int -> int
      val - : int * int -> int
      val remUnsafe: int * int -> int

      val < : int * int -> bool
      val <= : int * int -> bool
      val > : int * int -> bool
      val >= : int * int -> bool
      val compare: int * int -> Primitive.Order.order
      val min: int * int -> int
      val max: int * int -> int
      val ltu: int * int -> bool
      val leu: int * int -> bool
      val gtu: int * int -> bool
      val geu: int * int -> bool

      val andb: int * int -> int
      val <<? : int * Primitive.Word32.word -> int
      val notb: int -> int
      val orb: int * int -> int
      val rolUnsafe: int * Primitive.Word32.word -> int
      val rorUnsafe: int * Primitive.Word32.word -> int
      val ~>>? : int * Primitive.Word32.word -> int
      val >>? : int * Primitive.Word32.word -> int
      val xorb: int * int -> int

      (* Lowbits or sign extend. *)
      val fromInt8Unsafe: Primitive.Int8.int -> int
      val fromInt16Unsafe: Primitive.Int16.int -> int
      val fromInt32Unsafe: Primitive.Int32.int -> int
      val fromInt64Unsafe: Primitive.Int64.int -> int

(*
      (* Lowbits or zero extend. *)
      val fromInt8ZUnsafe: Primitive.Int8.int -> int
      val fromInt16ZUnsafe: Primitive.Int16.int -> int
      val fromInt32ZUnsafe: Primitive.Int32.int -> int
      val fromInt64ZUnsafe: Primitive.Int64.int -> int
*)

      (* Lowbits or zero extend. *)
      val fromWord8Unsafe: Primitive.Word8.word -> int
      val fromWord16Unsafe: Primitive.Word16.word -> int
      val fromWord32Unsafe: Primitive.Word32.word -> int
      val fromWord64Unsafe: Primitive.Word64.word -> int

      (* Lowbits or sign extend. *)
      val fromWord8XUnsafe: Primitive.Word8.word -> int
      val fromWord16XUnsafe: Primitive.Word16.word -> int
      val fromWord32XUnsafe: Primitive.Word32.word -> int
      val fromWord64XUnsafe: Primitive.Word64.word -> int

      (* Lowbits or sign extend. *)
      val toInt8Unsafe: int -> Primitive.Int8.int
      val toInt16Unsafe: int -> Primitive.Int16.int
      val toInt32Unsafe: int -> Primitive.Int32.int
      val toInt64Unsafe: int -> Primitive.Int64.int

(*
      (* Lowbits or zero extend. *)
      val toInt8ZUnsafe: int -> Primitive.Int8.int
      val toInt16ZUnsafe: int -> Primitive.Int16.int
      val toInt32ZUnsafe: int -> Primitive.Int32.int
      val toInt64ZUnsafe: int -> Primitive.Int64.int
*)

      (* Lowbits or zero extend. *)
      val toWord8Unsafe: int -> Primitive.Word8.word
      val toWord16Unsafe: int -> Primitive.Word16.word
      val toWord32Unsafe: int -> Primitive.Word32.word
      val toWord64Unsafe: int -> Primitive.Word64.word

      (* Lowbits or sign extend. *)
      val toWord8XUnsafe: int -> Primitive.Word8.word
      val toWord16XUnsafe: int -> Primitive.Word16.word
      val toWord32XUnsafe: int -> Primitive.Word32.word
      val toWord64XUnsafe: int -> Primitive.Word64.word
   end

structure Primitive = struct

open Primitive

structure Int1 =
   struct
      open Int1
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_toWord1": big -> int;
      val precision' : Int32.int = 1
      val toBig = _prim "WordU1_toWord8": int -> big;
   end
structure Int2 =
   struct
      open Int2
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_toWord2": big -> int;
      val precision' : Int32.int = 2
      val toBig = _prim "WordU2_toWord8": int -> big;
   end
structure Int3 =
   struct
      open Int3
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_toWord3": big -> int;
      val precision' : Int32.int = 3
      val toBig = _prim "WordU3_toWord8": int -> big;
   end
structure Int4 =
   struct
      open Int4
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_toWord4": big -> int;
      val precision' : Int32.int = 4
      val toBig = _prim "WordU4_toWord8": int -> big;
   end
structure Int5 =
   struct
      open Int5
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_toWord5": big -> int;
      val precision' : Int32.int = 5
      val toBig = _prim "WordU5_toWord8": int -> big;
   end
structure Int6 =
   struct
      open Int6
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_toWord6": big -> int;
      val precision' : Int32.int = 6
      val toBig = _prim "WordU6_toWord8": int -> big;
   end
structure Int7 =
   struct
      open Int7
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_toWord7": big -> int;
      val precision' : Int32.int = 7
      val toBig = _prim "WordU7_toWord8": int -> big;
   end
structure Int8 =
   struct
      open Int8
               
      val precision' : Int32.int = 8
      val maxInt' : int = 0x7f
      val minInt' : int = ~0x80
         
      val +? = _prim "Word8_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS8_addCheck": int * int -> int;)
            else +?
      val *? = _prim "WordS8_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS8_mulCheck": int * int -> int;)
            else *?
      val ~? = _prim "Word8_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "Word8_negCheck": int -> int;)
            else ~?
      val quotUnsafe = _prim "WordS8_quot": int * int -> int;
      val -? = _prim "Word8_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS8_subCheck": int * int -> int;)
            else -?
      val remUnsafe = _prim "WordS8_rem": int * int -> int;

      val < = _prim "WordS8_lt": int * int -> bool;

      val andb = _prim "Word8_andb": int * int -> int;
      val <<? = _prim "Word8_lshift": int * Word32.word -> int;
      val notb = _prim "Word8_notb": int -> int;
      val orb = _prim "Word8_orb": int * int -> int;
      val rolUnsafe = _prim "Word8_rol": int * Word32.word -> int;
      val rorUnsafe = _prim "Word8_ror": int * Word32.word -> int;
      val ~>>? = _prim "WordS8_rshift": int * Word32.word -> int;
      val >>? = _prim "WordU8_rshift": int * Word32.word -> int;
      val xorb = _prim "Word8_xorb": int * int -> int;

      val fromInt8Unsafe = _prim "WordS8_toWord8": Int8.int -> int;
      val fromInt16Unsafe = _prim "WordS16_toWord8": Int16.int -> int;
      val fromInt32Unsafe = _prim "WordS32_toWord8": Int32.int -> int;
      val fromInt64Unsafe = _prim "WordS64_toWord8": Int64.int -> int;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord8": Int8.int -> int;
      val fromInt16ZUnsafe = _prim "WordU16_toWord8": Int16.int -> int;
      val fromInt32ZUnsafe = _prim "WordU32_toWord8": Int32.int -> int;
      val fromInt64ZUnsafe = _prim "WordU64_toWord8": Int64.int -> int;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord8": Word8.word -> int;
      val fromWord16Unsafe = _prim "WordU16_toWord8": Word16.word -> int;
      val fromWord32Unsafe = _prim "WordU32_toWord8": Word32.word -> int;
      val fromWord64Unsafe = _prim "WordU64_toWord8": Word64.word -> int;

      val fromWord8XUnsafe = _prim "WordS8_toWord8": Word8.word -> int;
      val fromWord16XUnsafe = _prim "WordS16_toWord8": Word16.word -> int;
      val fromWord32XUnsafe = _prim "WordS32_toWord8": Word32.word -> int;
      val fromWord64XUnsafe = _prim "WordS64_toWord8": Word64.word -> int;

      val toInt8Unsafe = _prim "WordS8_toWord8": int -> Int8.int;
      val toInt16Unsafe = _prim "WordS8_toWord16": int -> Int16.int;
      val toInt32Unsafe = _prim "WordS8_toWord32": int -> Int32.int;
      val toInt64Unsafe = _prim "WordS8_toWord64": int -> Int64.int;

(*
      val toInt8ZUnsafe = _prim "WordU8_toWord8": int -> Int8.int;
      val toInt16ZUnsafe = _prim "WordU8_toWord16": int -> Int16.int;
      val toInt32ZUnsafe = _prim "WordU8_toWord32": int -> Int32.int;
      val toInt64ZUnsafe = _prim "WordU8_toWord64": int -> Int64.int;
*)

      val toWord8Unsafe = _prim "WordU8_toWord8": int -> Word8.word;
      val toWord16Unsafe = _prim "WordU8_toWord16": int -> Word16.word;
      val toWord32Unsafe = _prim "WordU8_toWord32": int -> Word32.word;
      val toWord64Unsafe = _prim "WordU8_toWord64": int -> Word64.word;

      val toWord8XUnsafe = _prim "WordS8_toWord8": int -> Word8.word;
      val toWord16XUnsafe = _prim "WordS8_toWord16": int -> Word16.word;
      val toWord32XUnsafe = _prim "WordS8_toWord32": int -> Word32.word;
      val toWord64XUnsafe = _prim "WordS8_toWord64": int -> Word64.word;
   end
structure Int8 : PRIM_INTEGER =
   struct
      open Int8
      local
         structure S = IntegralComparisons(Int8)
      in
         open S
      end
      local
         structure S = UnsignedIntegralComparisons(type int = Int8.int
                                                   type word = Word8.word
                                                   val fromInt = Word8.fromInt8Unsafe
                                                   val < = Word8.<)
      in
         open S
      end
   end
structure Int9 =
   struct
      open Int9
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_toWord9": big -> int;
      val precision' : Int32.int = 9
      val toBig = _prim "WordU9_toWord16": int -> big;
   end
structure Int10 =
   struct
      open Int10
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_toWord10": big -> int;
      val precision' : Int32.int = 10
      val toBig = _prim "WordU10_toWord16": int -> big;
   end
structure Int11 =
   struct
      open Int11
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_toWord11": big -> int;
      val precision' : Int32.int = 11
      val toBig = _prim "WordU11_toWord16": int -> big;
   end
structure Int12 =
   struct
      open Int12
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_toWord12": big -> int;
      val precision' : Int32.int = 12
      val toBig = _prim "WordU12_toWord16": int -> big;
   end
structure Int13 =
   struct
      open Int13
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_toWord13": big -> int;
      val precision' : Int32.int = 13
      val toBig = _prim "WordU13_toWord16": int -> big;
   end
structure Int14 =
   struct
      open Int14
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_toWord14": big -> int;
      val precision' : Int32.int = 14
      val toBig = _prim "WordU14_toWord16": int -> big;
   end
structure Int15 =
   struct
      open Int15
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_toWord15": big -> int;
      val precision' : Int32.int = 15
      val toBig = _prim "WordU15_toWord16": int -> big;
   end
structure Int16 =
   struct
      open Int16
               
      val precision' : Int32.int = 16
      val maxInt' : int = 0x7fff
      val minInt' : int = ~0x8000
         
      val +? = _prim "Word16_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS16_addCheck": int * int -> int;)
            else +?
      val *? = _prim "WordS16_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS16_mulCheck": int * int -> int;)
            else *?
      val ~? = _prim "Word16_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "Word16_negCheck": int -> int;)
            else ~?
      val quotUnsafe = _prim "WordS16_quot": int * int -> int;
      val -? = _prim "Word16_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS16_subCheck": int * int -> int;)
            else -?
      val remUnsafe = _prim "WordS16_rem": int * int -> int;

      val < = _prim "WordS16_lt": int * int -> bool;

      val andb = _prim "Word16_andb": int * int -> int;
      val <<? = _prim "Word16_lshift": int * Word32.word -> int;
      val notb = _prim "Word16_notb": int -> int;
      val orb = _prim "Word16_orb": int * int -> int;
      val rolUnsafe = _prim "Word16_rol": int * Word32.word -> int;
      val rorUnsafe = _prim "Word16_ror": int * Word32.word -> int;
      val ~>>? = _prim "WordS16_rshift": int * Word32.word -> int;
      val >>? = _prim "WordU16_rshift": int * Word32.word -> int;
      val xorb = _prim "Word16_xorb": int * int -> int;

      val fromInt8Unsafe = _prim "WordS8_toWord16": Int8.int -> int;
      val fromInt16Unsafe = _prim "WordS16_toWord16": Int16.int -> int;
      val fromInt32Unsafe = _prim "WordS32_toWord16": Int32.int -> int;
      val fromInt64Unsafe = _prim "WordS64_toWord16": Int64.int -> int;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord16": Int8.int -> int;
      val fromInt16ZUnsafe = _prim "WordU16_toWord16": Int16.int -> int;
      val fromInt32ZUnsafe = _prim "WordU32_toWord16": Int32.int -> int;
      val fromInt64ZUnsafe = _prim "WordU64_toWord16": Int64.int -> int;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord16": Word8.word -> int;
      val fromWord16Unsafe = _prim "WordU16_toWord16": Word16.word -> int;
      val fromWord32Unsafe = _prim "WordU32_toWord16": Word32.word -> int;
      val fromWord64Unsafe = _prim "WordU64_toWord16": Word64.word -> int;

      val fromWord8XUnsafe = _prim "WordS8_toWord16": Word8.word -> int;
      val fromWord16XUnsafe = _prim "WordS16_toWord16": Word16.word -> int;
      val fromWord32XUnsafe = _prim "WordS32_toWord16": Word32.word -> int;
      val fromWord64XUnsafe = _prim "WordS64_toWord16": Word64.word -> int;

      val toInt8Unsafe = _prim "WordS16_toWord8": int -> Int8.int;
      val toInt16Unsafe = _prim "WordS16_toWord16": int -> Int16.int;
      val toInt32Unsafe = _prim "WordS16_toWord32": int -> Int32.int;
      val toInt64Unsafe = _prim "WordS16_toWord64": int -> Int64.int;

(*
      val toInt8ZUnsafe = _prim "WordU16_toWord8": int -> Int8.int;
      val toInt16ZUnsafe = _prim "WordU16_toWord16": int -> Int16.int;
      val toInt32ZUnsafe = _prim "WordU16_toWord32": int -> Int32.int;
      val toInt64ZUnsafe = _prim "WordU16_toWord64": int -> Int64.int;
*)

      val toWord8Unsafe = _prim "WordU16_toWord8": int -> Word8.word;
      val toWord16Unsafe = _prim "WordU16_toWord16": int -> Word16.word;
      val toWord32Unsafe = _prim "WordU16_toWord32": int -> Word32.word;
      val toWord64Unsafe = _prim "WordU16_toWord64": int -> Word64.word;

      val toWord8XUnsafe = _prim "WordS16_toWord8": int -> Word8.word;
      val toWord16XUnsafe = _prim "WordS16_toWord16": int -> Word16.word;
      val toWord32XUnsafe = _prim "WordS16_toWord32": int -> Word32.word;
      val toWord64XUnsafe = _prim "WordS16_toWord64": int -> Word64.word;
   end
structure Int16 : PRIM_INTEGER =
   struct
      open Int16
      local
         structure S = IntegralComparisons(Int16)
      in
         open S
      end
      local
         structure S = UnsignedIntegralComparisons(type int = Int16.int
                                                   type word = Word16.word
                                                   val fromInt = Word16.fromInt16Unsafe
                                                   val < = Word16.<)
      in
         open S
      end
   end
structure Int17 =
   struct
      open Int17
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord17": big -> int;
      val precision' : Int32.int = 17
      val toBig = _prim "WordU17_toWord32": int -> big;
   end
structure Int18 =
   struct
      open Int18
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord18": big -> int;
      val precision' : Int32.int = 18
      val toBig = _prim "WordU18_toWord32": int -> big;
   end
structure Int19 =
   struct
      open Int19
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord19": big -> int;
      val precision' : Int32.int = 19
      val toBig = _prim "WordU19_toWord32": int -> big;
   end
structure Int20 =
   struct
      open Int20
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord20": big -> int;
      val precision' : Int32.int = 20
      val toBig = _prim "WordU20_toWord32": int -> big;
   end
structure Int21 =
   struct
      open Int21
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord21": big -> int;
      val precision' : Int32.int = 21
      val toBig = _prim "WordU21_toWord32": int -> big;
   end
structure Int22 =
   struct
      open Int22
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord22": big -> int;
      val precision' : Int32.int = 22
      val toBig = _prim "WordU22_toWord32": int -> big;
   end
structure Int23 =
   struct
      open Int23
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord23": big -> int;
      val precision' : Int32.int = 23
      val toBig = _prim "WordU23_toWord32": int -> big;
   end
structure Int24 =
   struct
      open Int24
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord24": big -> int;
      val precision' : Int32.int = 24
      val toBig = _prim "WordU24_toWord32": int -> big;
   end
structure Int25 =
   struct
      open Int25
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord25": big -> int;
      val precision' : Int32.int = 25
      val toBig = _prim "WordU25_toWord32": int -> big;
   end
structure Int26 =
   struct
      open Int26
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord26": big -> int;
      val precision' : Int32.int = 26
      val toBig = _prim "WordU26_toWord32": int -> big;
   end
structure Int27 =
   struct
      open Int27
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord27": big -> int;
      val precision' : Int32.int = 27
      val toBig = _prim "WordU27_toWord32": int -> big;
   end
structure Int28 =
   struct
      open Int28
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord28": big -> int;
      val precision' : Int32.int = 28
      val toBig = _prim "WordU28_toWord32": int -> big;
   end
structure Int29 =
   struct
      open Int29
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord29": big -> int;
      val precision' : Int32.int = 29
      val toBig = _prim "WordU29_toWord32": int -> big;
   end
structure Int30 =
   struct
      open Int30
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord30": big -> int;
      val precision' : Int32.int = 30
      val toBig = _prim "WordU30_toWord32": int -> big;
   end
structure Int31 =
   struct
      open Int31
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_toWord31": big -> int;
      val precision' : Int32.int = 31
      val toBig = _prim "WordU31_toWord32": int -> big;
   end
structure Int32 =
   struct
      open Int32
               
      val precision' : Int32.int = 32
      val maxInt' : int = 0x7fffffff
      val minInt' : int = ~0x80000000
         
      val +? = _prim "Word32_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS32_addCheck": int * int -> int;)
            else +?
      val *? = _prim "WordS32_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS32_mulCheck": int * int -> int;)
            else *?
      val ~? = _prim "Word32_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "Word32_negCheck": int -> int;)
            else ~?
      val quotUnsafe = _prim "WordS32_quot": int * int -> int;
      val -? = _prim "Word32_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS32_subCheck": int * int -> int;)
            else -?
      val remUnsafe = _prim "WordS32_rem": int * int -> int;

      val < = _prim "WordS32_lt": int * int -> bool;

      val andb = _prim "Word32_andb": int * int -> int;
      val <<? = _prim "Word32_lshift": int * Word32.word -> int;
      val notb = _prim "Word32_notb": int -> int;
      val orb = _prim "Word32_orb": int * int -> int;
      val rolUnsafe = _prim "Word32_rol": int * Word32.word -> int;
      val rorUnsafe = _prim "Word32_ror": int * Word32.word -> int;
      val ~>>? = _prim "WordS32_rshift": int * Word32.word -> int;
      val >>? = _prim "WordU32_rshift": int * Word32.word -> int;
      val xorb = _prim "Word32_xorb": int * int -> int;

      val fromInt8Unsafe = _prim "WordS8_toWord32": Int8.int -> int;
      val fromInt16Unsafe = _prim "WordS16_toWord32": Int16.int -> int;
      val fromInt32Unsafe = _prim "WordS32_toWord32": Int32.int -> int;
      val fromInt64Unsafe = _prim "WordS64_toWord32": Int64.int -> int;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord32": Int8.int -> int;
      val fromInt16ZUnsafe = _prim "WordU16_toWord32": Int16.int -> int;
      val fromInt32ZUnsafe = _prim "WordU32_toWord32": Int32.int -> int;
      val fromInt64ZUnsafe = _prim "WordU64_toWord32": Int64.int -> int;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord32": Word8.word -> int;
      val fromWord16Unsafe = _prim "WordU16_toWord32": Word16.word -> int;
      val fromWord32Unsafe = _prim "WordU32_toWord32": Word32.word -> int;
      val fromWord64Unsafe = _prim "WordU64_toWord32": Word64.word -> int;

      val fromWord8XUnsafe = _prim "WordS8_toWord32": Word8.word -> int;
      val fromWord16XUnsafe = _prim "WordS16_toWord32": Word16.word -> int;
      val fromWord32XUnsafe = _prim "WordS32_toWord32": Word32.word -> int;
      val fromWord64XUnsafe = _prim "WordS64_toWord32": Word64.word -> int;

      val toInt8Unsafe = _prim "WordS32_toWord8": int -> Int8.int;
      val toInt16Unsafe = _prim "WordS32_toWord16": int -> Int16.int;
      val toInt32Unsafe = _prim "WordS32_toWord32": int -> Int32.int;
      val toInt64Unsafe = _prim "WordS32_toWord64": int -> Int64.int;

(*
      val toInt8ZUnsafe = _prim "WordU32_toWord8": int -> Int8.int;
      val toInt16ZUnsafe = _prim "WordU32_toWord16": int -> Int16.int;
      val toInt32ZUnsafe = _prim "WordU32_toWord32": int -> Int32.int;
      val toInt64ZUnsafe = _prim "WordU32_toWord64": int -> Int64.int;
*)

      val toWord8Unsafe = _prim "WordU32_toWord8": int -> Word8.word;
      val toWord16Unsafe = _prim "WordU32_toWord16": int -> Word16.word;
      val toWord32Unsafe = _prim "WordU32_toWord32": int -> Word32.word;
      val toWord64Unsafe = _prim "WordU32_toWord64": int -> Word64.word;

      val toWord8XUnsafe = _prim "WordS32_toWord8": int -> Word8.word;
      val toWord16XUnsafe = _prim "WordS32_toWord16": int -> Word16.word;
      val toWord32XUnsafe = _prim "WordS32_toWord32": int -> Word32.word;
      val toWord64XUnsafe = _prim "WordS32_toWord64": int -> Word64.word;
   end
structure Int32 : PRIM_INTEGER =
   struct
      open Int32
      local
         structure S = IntegralComparisons(Int32)
      in
         open S
      end
      local
         structure S = UnsignedIntegralComparisons(type int = Int32.int
                                                   type word = Word32.word
                                                   val fromInt = Word32.fromInt32Unsafe
                                                   val < = Word32.<)
      in
         open S
      end
   end
structure Int64 =
   struct
      open Int64
               
      val precision' : Int32.int = 64
      val maxInt' : int = 0x7fffffffffffffff
      val minInt' : int = ~0x8000000000000000
         
      val +? = _prim "Word64_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS64_addCheck": int * int -> int;)
            else +?
      val *? = _prim "WordS64_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS64_mulCheck": int * int -> int;)
            else *?
      val ~? = _prim "Word64_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "Word64_negCheck": int -> int;)
            else ~?
      val quotUnsafe = _prim "WordS64_quot": int * int -> int;
      val -? = _prim "Word64_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then Exn.wrapOverflow (_prim "WordS64_subCheck": int * int -> int;)
            else -?
      val remUnsafe = _prim "WordS64_rem": int * int -> int;

      val < = _prim "WordS64_lt": int * int -> bool;

      val andb = _prim "Word64_andb": int * int -> int;
      val <<? = _prim "Word64_lshift": int * Word32.word -> int;
      val notb = _prim "Word64_notb": int -> int;
      val orb = _prim "Word64_orb": int * int -> int;
      val rolUnsafe = _prim "Word64_rol": int * Word32.word -> int;
      val rorUnsafe = _prim "Word64_ror": int * Word32.word -> int;
      val ~>>? = _prim "WordS64_rshift": int * Word32.word -> int;
      val >>? = _prim "WordU64_rshift": int * Word32.word -> int;
      val xorb = _prim "Word64_xorb": int * int -> int;

      val fromInt8Unsafe = _prim "WordS8_toWord64": Int8.int -> int;
      val fromInt16Unsafe = _prim "WordS16_toWord64": Int16.int -> int;
      val fromInt32Unsafe = _prim "WordS32_toWord64": Int32.int -> int;
      val fromInt64Unsafe = _prim "WordS64_toWord64": Int64.int -> int;

(*
      val fromInt8ZUnsafe = _prim "WordU8_toWord64": Int8.int -> int;
      val fromInt16ZUnsafe = _prim "WordU16_toWord64": Int16.int -> int;
      val fromInt32ZUnsafe = _prim "WordU32_toWord64": Int32.int -> int;
      val fromInt64ZUnsafe = _prim "WordU64_toWord64": Int64.int -> int;
*)

      val fromWord8Unsafe = _prim "WordU8_toWord64": Word8.word -> int;
      val fromWord16Unsafe = _prim "WordU16_toWord64": Word16.word -> int;
      val fromWord32Unsafe = _prim "WordU32_toWord64": Word32.word -> int;
      val fromWord64Unsafe = _prim "WordU64_toWord64": Word64.word -> int;

      val fromWord8XUnsafe = _prim "WordS8_toWord64": Word8.word -> int;
      val fromWord16XUnsafe = _prim "WordS16_toWord64": Word16.word -> int;
      val fromWord32XUnsafe = _prim "WordS32_toWord64": Word32.word -> int;
      val fromWord64XUnsafe = _prim "WordS64_toWord64": Word64.word -> int;

      val toInt8Unsafe = _prim "WordS64_toWord8": int -> Int8.int;
      val toInt16Unsafe = _prim "WordS64_toWord16": int -> Int16.int;
      val toInt32Unsafe = _prim "WordS64_toWord32": int -> Int32.int;
      val toInt64Unsafe = _prim "WordS64_toWord64": int -> Int64.int;

(*
      val toInt8ZUnsafe = _prim "WordU64_toWord8": int -> Int8.int;
      val toInt16ZUnsafe = _prim "WordU64_toWord16": int -> Int16.int;
      val toInt32ZUnsafe = _prim "WordU64_toWord32": int -> Int32.int;
      val toInt64ZUnsafe = _prim "WordU64_toWord64": int -> Int64.int;
*)

      val toWord8Unsafe = _prim "WordU64_toWord8": int -> Word8.word;
      val toWord16Unsafe = _prim "WordU64_toWord16": int -> Word16.word;
      val toWord32Unsafe = _prim "WordU64_toWord32": int -> Word32.word;
      val toWord64Unsafe = _prim "WordU64_toWord64": int -> Word64.word;

      val toWord8XUnsafe = _prim "WordS64_toWord8": int -> Word8.word;
      val toWord16XUnsafe = _prim "WordS64_toWord16": int -> Word16.word;
      val toWord32XUnsafe = _prim "WordS64_toWord32": int -> Word32.word;
      val toWord64XUnsafe = _prim "WordS64_toWord64": int -> Word64.word;
   end
structure Int64 : PRIM_INTEGER =
   struct
      open Int64
      local
         structure S = IntegralComparisons(Int64)
      in
         open S
      end
      local
         structure S = UnsignedIntegralComparisons(type int = Int64.int
                                                   type word = Word64.word
                                                   val fromInt = Word64.fromInt64Unsafe
                                                   val < = Word64.<)
      in
         open S
      end
   end

end
