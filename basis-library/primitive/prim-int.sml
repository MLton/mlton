(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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

      val sizeInBits: Primitive.Int32.int
      val sizeInBitsWord: Primitive.Word32.word
      val precision: Primitive.Int32.int option

      val +! : int * int -> int
      val +? : int * int -> int
      val + : int * int -> int
      val *! : int * int -> int 
      val *? : int * int -> int
      val * : int * int -> int
      val ~! : int -> int
      val ~? : int -> int
      val ~ : int -> int
      val quotUnsafe: int * int -> int
      val -! : int * int -> int
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
   end

structure Primitive = struct

open Primitive

structure Int1 =
   struct
      open Int1
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_extdToWord1": big -> int;
      val sizeInBits: Int32.int = 1
      val toBig = _prim "WordU1_extdToWord8": int -> big;
   end
structure Int2 =
   struct
      open Int2
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_extdToWord2": big -> int;
      val sizeInBits: Int32.int = 2
      val toBig = _prim "WordU2_extdToWord8": int -> big;
   end
structure Int3 =
   struct
      open Int3
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_extdToWord3": big -> int;
      val sizeInBits: Int32.int = 3
      val toBig = _prim "WordU3_extdToWord8": int -> big;
   end
structure Int4 =
   struct
      open Int4
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_extdToWord4": big -> int;
      val sizeInBits: Int32.int = 4
      val toBig = _prim "WordU4_extdToWord8": int -> big;
   end
structure Int5 =
   struct
      open Int5
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_extdToWord5": big -> int;
      val sizeInBits: Int32.int = 5
      val toBig = _prim "WordU5_extdToWord8": int -> big;
   end
structure Int6 =
   struct
      open Int6
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_extdToWord6": big -> int;
      val sizeInBits: Int32.int = 6
      val toBig = _prim "WordU6_extdToWord8": int -> big;
   end
structure Int7 =
   struct
      open Int7
      type big = Int8.int
      val fromBigUnsafe = _prim "WordU8_extdToWord7": big -> int;
      val sizeInBits: Int32.int = 7
      val toBig = _prim "WordU7_extdToWord8": int -> big;
   end
structure Int8 =
   struct
      open Int8

      val sizeInBits: Int32.int = 8
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits
      val precision = SOME sizeInBits

      val +! = Exn.wrapOverflow (_prim "WordS8_addCheck": int * int -> int;)
      val +? = _prim "Word8_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then +!
            else +?
      val *! = Exn.wrapOverflow (_prim "WordS8_mulCheck": int * int -> int;)
      val *? = _prim "WordS8_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then *!
            else *?
      val ~! = Exn.wrapOverflow (_prim "Word8_negCheck": int -> int;)
      val ~? = _prim "Word8_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then ~!
            else ~?
      val quotUnsafe = _prim "WordS8_quot": int * int -> int;
      val -! = Exn.wrapOverflow (_prim "WordS8_subCheck": int * int -> int;)
      val -? = _prim "Word8_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then -!
            else -?
      val remUnsafe = _prim "WordS8_rem": int * int -> int;

      val < = _prim "WordS8_lt": int * int -> bool;
   end
structure Int8 : PRIM_INTEGER =
   struct
      open Int8
      local
         structure S = IntegralComparisons(Int8)
      in
         open S
      end
   end
structure Int9 =
   struct
      open Int9
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_extdToWord9": big -> int;
      val sizeInBits: Int32.int = 9
      val toBig = _prim "WordU9_extdToWord16": int -> big;
   end
structure Int10 =
   struct
      open Int10
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_extdToWord10": big -> int;
      val sizeInBits: Int32.int = 10
      val toBig = _prim "WordU10_extdToWord16": int -> big;
   end
structure Int11 =
   struct
      open Int11
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_extdToWord11": big -> int;
      val sizeInBits: Int32.int = 11
      val toBig = _prim "WordU11_extdToWord16": int -> big;
   end
structure Int12 =
   struct
      open Int12
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_extdToWord12": big -> int;
      val sizeInBits: Int32.int = 12
      val toBig = _prim "WordU12_extdToWord16": int -> big;
   end
structure Int13 =
   struct
      open Int13
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_extdToWord13": big -> int;
      val sizeInBits: Int32.int = 13
      val toBig = _prim "WordU13_extdToWord16": int -> big;
   end
structure Int14 =
   struct
      open Int14
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_extdToWord14": big -> int;
      val sizeInBits: Int32.int = 14
      val toBig = _prim "WordU14_extdToWord16": int -> big;
   end
structure Int15 =
   struct
      open Int15
      type big = Int16.int
      val fromBigUnsafe = _prim "WordU16_extdToWord15": big -> int;
      val sizeInBits: Int32.int = 15
      val toBig = _prim "WordU15_extdToWord16": int -> big;
   end
structure Int16 =
   struct
      open Int16

      val sizeInBits: Int32.int = 16
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits
      val precision = SOME sizeInBits

      val +! = Exn.wrapOverflow (_prim "WordS16_addCheck": int * int -> int;)
      val +? = _prim "Word16_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then +!
            else +?
      val *! = Exn.wrapOverflow (_prim "WordS16_mulCheck": int * int -> int;)
      val *? = _prim "WordS16_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then *!
            else *?
      val ~! = Exn.wrapOverflow (_prim "Word16_negCheck": int -> int;)
      val ~? = _prim "Word16_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then ~!
            else ~?
      val quotUnsafe = _prim "WordS16_quot": int * int -> int;
      val -! = Exn.wrapOverflow (_prim "WordS16_subCheck": int * int -> int;)
      val -? = _prim "Word16_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then -!
            else -?
      val remUnsafe = _prim "WordS16_rem": int * int -> int;

      val < = _prim "WordS16_lt": int * int -> bool;
   end
structure Int16 : PRIM_INTEGER =
   struct
      open Int16
      local
         structure S = IntegralComparisons(Int16)
      in
         open S
      end
   end
structure Int17 =
   struct
      open Int17
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord17": big -> int;
      val sizeInBits: Int32.int = 17
      val toBig = _prim "WordU17_extdToWord32": int -> big;
   end
structure Int18 =
   struct
      open Int18
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord18": big -> int;
      val sizeInBits: Int32.int = 18
      val toBig = _prim "WordU18_extdToWord32": int -> big;
   end
structure Int19 =
   struct
      open Int19
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord19": big -> int;
      val sizeInBits: Int32.int = 19
      val toBig = _prim "WordU19_extdToWord32": int -> big;
   end
structure Int20 =
   struct
      open Int20
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord20": big -> int;
      val sizeInBits: Int32.int = 20
      val toBig = _prim "WordU20_extdToWord32": int -> big;
   end
structure Int21 =
   struct
      open Int21
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord21": big -> int;
      val sizeInBits: Int32.int = 21
      val toBig = _prim "WordU21_extdToWord32": int -> big;
   end
structure Int22 =
   struct
      open Int22
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord22": big -> int;
      val sizeInBits: Int32.int = 22
      val toBig = _prim "WordU22_extdToWord32": int -> big;
   end
structure Int23 =
   struct
      open Int23
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord23": big -> int;
      val sizeInBits: Int32.int = 23
      val toBig = _prim "WordU23_extdToWord32": int -> big;
   end
structure Int24 =
   struct
      open Int24
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord24": big -> int;
      val sizeInBits: Int32.int = 24
      val toBig = _prim "WordU24_extdToWord32": int -> big;
   end
structure Int25 =
   struct
      open Int25
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord25": big -> int;
      val sizeInBits: Int32.int = 25
      val toBig = _prim "WordU25_extdToWord32": int -> big;
   end
structure Int26 =
   struct
      open Int26
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord26": big -> int;
      val sizeInBits: Int32.int = 26
      val toBig = _prim "WordU26_extdToWord32": int -> big;
   end
structure Int27 =
   struct
      open Int27
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord27": big -> int;
      val sizeInBits: Int32.int = 27
      val toBig = _prim "WordU27_extdToWord32": int -> big;
   end
structure Int28 =
   struct
      open Int28
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord28": big -> int;
      val sizeInBits: Int32.int = 28
      val toBig = _prim "WordU28_extdToWord32": int -> big;
   end
structure Int29 =
   struct
      open Int29
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord29": big -> int;
      val sizeInBits: Int32.int = 29
      val toBig = _prim "WordU29_extdToWord32": int -> big;
   end
structure Int30 =
   struct
      open Int30
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord30": big -> int;
      val sizeInBits: Int32.int = 30
      val toBig = _prim "WordU30_extdToWord32": int -> big;
   end
structure Int31 =
   struct
      open Int31
      type big = Int32.int
      val fromBigUnsafe = _prim "WordU32_extdToWord31": big -> int;
      val sizeInBits: Int32.int = 31
      val toBig = _prim "WordU31_extdToWord32": int -> big;
   end
structure Int32 =
   struct
      open Int32

      val sizeInBits: Int32.int = 32
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits
      val precision = SOME sizeInBits

      val +! = Exn.wrapOverflow (_prim "WordS32_addCheck": int * int -> int;)
      val +? = _prim "Word32_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then +!
            else +?
      val *! = Exn.wrapOverflow (_prim "WordS32_mulCheck": int * int -> int;)
      val *? = _prim "WordS32_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then *!
            else *?
      val ~! = Exn.wrapOverflow (_prim "Word32_negCheck": int -> int;)
      val ~? = _prim "Word32_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then ~!
            else ~?
      val quotUnsafe = _prim "WordS32_quot": int * int -> int;
      val -! = Exn.wrapOverflow (_prim "WordS32_subCheck": int * int -> int;)
      val -? = _prim "Word32_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then -!
            else -?
      val remUnsafe = _prim "WordS32_rem": int * int -> int;

      val < = _prim "WordS32_lt": int * int -> bool;
   end
structure Int32 : PRIM_INTEGER =
   struct
      open Int32
      local
         structure S = IntegralComparisons(Int32)
      in
         open S
      end
   end
structure Int64 =
   struct
      open Int64

      val sizeInBits: Int32.int = 64
      val sizeInBitsWord: Word32.word = 
         IntWordConv.zextdFromInt32ToWord32 sizeInBits
      val precision = SOME sizeInBits

      val +! = Exn.wrapOverflow (_prim "WordS64_addCheck": int * int -> int;)
      val +? = _prim "Word64_add": int * int -> int;
      val + =
         if Controls.detectOverflow
            then +!
            else +?
      val *! = Exn.wrapOverflow (_prim "WordS64_mulCheck": int * int -> int;)
      val *? = _prim "WordS64_mul": int * int -> int;
      val * =
         if Controls.detectOverflow
            then *!
            else *?
      val ~! = Exn.wrapOverflow (_prim "Word64_negCheck": int -> int;)
      val ~? = _prim "Word64_neg": int -> int; 
      val ~ =
         if Controls.detectOverflow
            then ~!
            else ~?
      val quotUnsafe = _prim "WordS64_quot": int * int -> int;
      val -! = Exn.wrapOverflow (_prim "WordS64_subCheck": int * int -> int;)
      val -? = _prim "Word64_sub": int * int -> int;
      val - =
         if Controls.detectOverflow
            then -!
            else -?
      val remUnsafe = _prim "WordS64_rem": int * int -> int;

      val < = _prim "WordS64_lt": int * int -> bool;
   end
structure Int64 : PRIM_INTEGER =
   struct
      open Int64
      local
         structure S = IntegralComparisons(Int64)
      in
         open S
      end
   end

end
