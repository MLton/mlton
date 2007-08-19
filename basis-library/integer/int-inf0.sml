(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_INT_INF =
   sig
      eqtype int
      type t = int

      val precision: Primitive.Int32.int option

      val maxInt: int option
      val minInt: int option

      val zero: int
      val one: int
      val negOne: int

      datatype rep =
         Big of C_MPLimb.word vector
       | Small of ObjptrInt.int
      val rep: int -> rep

      structure Prim : 
         sig
            val isSmall: int -> bool
            val areSmall: int * int -> bool
            val dropTag: ObjptrWord.word -> ObjptrWord.word
            val dropTagCoerce: int -> ObjptrWord.word
            val dropTagCoerceInt: int -> ObjptrInt.int
            val addTag: ObjptrWord.word -> ObjptrWord.word
            val addTagCoerce: ObjptrWord.word -> int
            val addTagCoerceInt: ObjptrInt.int -> int
            val zeroTag: ObjptrWord.word -> ObjptrWord.word
            val oneTag: ObjptrWord.word -> ObjptrWord.word
            val oneTagCoerce: ObjptrWord.word -> int
         end

      val abs: int -> int
      val +! : int * int -> int
      val +? : int * int -> int
      val + : int * int -> int
      val divMod: int * int -> int * int
      val div: int * int -> int
      val gcd: int * int -> int
      val mod: int * int -> int
      val *! : int * int -> int
      val *? : int * int -> int
      val * : int * int -> int
      val ~! : int -> int
      val ~? : int -> int
      val ~ : int -> int
      val quotRem: int * int -> int * int
      val quot: int * int -> int
      val rem: int * int -> int
      val -! : int * int -> int
      val -? : int * int -> int
      val - : int * int -> int

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
      val isNeg: int -> bool

      val andb: int * int -> int
      val <<? : int * Primitive.Word32.word -> int
      val << : int * Primitive.Word32.word -> int
      val notb: int -> int
      val orb: int * int -> int
      val ~>>? : int * Primitive.Word32.word -> int
      val ~>> : int * Primitive.Word32.word -> int
      val xorb: int * int -> int

      val mkCvt: ({base: Primitive.Int32.int,
                   smallCvt: ObjptrInt.int -> Primitive.String8.string} 
                  -> int -> Primitive.String8.string)
      val mkLog2: ({fromSmall: {smallLog2: Primitive.Int32.int} -> 'a,
                    fromLarge: {mostSigLimbLog2: Primitive.Int32.int,
                                numLimbsMinusOne: SeqIndex.int} -> 'a}
                   -> int -> 'a)

      val zextdFromInt8: Primitive.Int8.int -> int
      val zextdFromInt16: Primitive.Int16.int -> int
      val zextdFromInt32: Primitive.Int32.int -> int
      val zextdFromInt64: Primitive.Int64.int -> int
      val zextdFromIntInf: Primitive.IntInf.int -> int
      val zextdFromWord8: Primitive.Word8.word -> int
      val zextdFromWord16: Primitive.Word16.word -> int
      val zextdFromWord32: Primitive.Word32.word -> int
      val zextdFromWord64: Primitive.Word64.word -> int
      val zextdToInt8: int -> Primitive.Int8.int
      val zextdToInt16: int -> Primitive.Int16.int
      val zextdToInt32: int -> Primitive.Int32.int
      val zextdToInt64: int -> Primitive.Int64.int
      val zextdToIntInf: int -> Primitive.IntInf.int
      val zextdToWord8: int -> Primitive.Word8.word
      val zextdToWord16: int -> Primitive.Word16.word
      val zextdToWord32: int -> Primitive.Word32.word
      val zextdToWord64: int -> Primitive.Word64.word

      val sextdFromInt8: Primitive.Int8.int -> int
      val sextdFromInt16: Primitive.Int16.int -> int
      val sextdFromInt32: Primitive.Int32.int -> int
      val sextdFromInt64: Primitive.Int64.int -> int
      val sextdFromIntInf: Primitive.IntInf.int -> int
      val sextdFromWord8: Primitive.Word8.word -> int
      val sextdFromWord16: Primitive.Word16.word -> int
      val sextdFromWord32: Primitive.Word32.word -> int
      val sextdFromWord64: Primitive.Word64.word -> int
      val sextdToInt8: int -> Primitive.Int8.int
      val sextdToInt16: int -> Primitive.Int16.int
      val sextdToInt32: int -> Primitive.Int32.int
      val sextdToInt64: int -> Primitive.Int64.int
      val sextdToIntInf: int -> Primitive.IntInf.int
      val sextdToWord8: int -> Primitive.Word8.word
      val sextdToWord16: int -> Primitive.Word16.word
      val sextdToWord32: int -> Primitive.Word32.word
      val sextdToWord64: int -> Primitive.Word64.word

      val castFromInt8: Primitive.Int8.int -> int
      val castFromInt16: Primitive.Int16.int -> int
      val castFromInt32: Primitive.Int32.int -> int
      val castFromInt64: Primitive.Int64.int -> int
      val castFromIntInf: Primitive.IntInf.int -> int
      val castFromWord8: Primitive.Word8.word -> int
      val castFromWord16: Primitive.Word16.word -> int
      val castFromWord32: Primitive.Word32.word -> int
      val castFromWord64: Primitive.Word64.word -> int
      val castToInt8: int -> Primitive.Int8.int
      val castToInt16: int -> Primitive.Int16.int
      val castToInt32: int -> Primitive.Int32.int
      val castToInt64: int -> Primitive.Int64.int
      val castToIntInf: int -> Primitive.IntInf.int
      val castToWord8: int -> Primitive.Word8.word
      val castToWord16: int -> Primitive.Word16.word
      val castToWord32: int -> Primitive.Word32.word
      val castToWord64: int -> Primitive.Word64.word

      val zchckFromInt8: Primitive.Int8.int -> int
      val zchckFromInt16: Primitive.Int16.int -> int
      val zchckFromInt32: Primitive.Int32.int -> int
      val zchckFromInt64: Primitive.Int64.int -> int
      val zchckFromIntInf: Primitive.IntInf.int -> int
      val zchckFromWord8: Primitive.Word8.word -> int
      val zchckFromWord16: Primitive.Word16.word -> int
      val zchckFromWord32: Primitive.Word32.word -> int
      val zchckFromWord64: Primitive.Word64.word -> int
      val zchckToInt8: int -> Primitive.Int8.int
      val zchckToInt16: int -> Primitive.Int16.int
      val zchckToInt32: int -> Primitive.Int32.int
      val zchckToInt64: int -> Primitive.Int64.int
      val zchckToIntInf: int -> Primitive.IntInf.int
      val zchckToWord8: int -> Primitive.Word8.word
      val zchckToWord16: int -> Primitive.Word16.word
      val zchckToWord32: int -> Primitive.Word32.word
      val zchckToWord64: int -> Primitive.Word64.word

      val schckFromInt8: Primitive.Int8.int -> int
      val schckFromInt16: Primitive.Int16.int -> int
      val schckFromInt32: Primitive.Int32.int -> int
      val schckFromInt64: Primitive.Int64.int -> int
      val schckFromIntInf: Primitive.IntInf.int -> int
      val schckFromWord8: Primitive.Word8.word -> int
      val schckFromWord16: Primitive.Word16.word -> int
      val schckFromWord32: Primitive.Word32.word -> int
      val schckFromWord64: Primitive.Word64.word -> int
      val schckToInt8: int -> Primitive.Int8.int
      val schckToInt16: int -> Primitive.Int16.int
      val schckToInt32: int -> Primitive.Int32.int
      val schckToInt64: int -> Primitive.Int64.int
      val schckToIntInf: int -> Primitive.IntInf.int
      val schckToWord8: int -> Primitive.Word8.word
      val schckToWord16: int -> Primitive.Word16.word
      val schckToWord32: int -> Primitive.Word32.word
      val schckToWord64: int -> Primitive.Word64.word
   end

signature PRIM_INTWORD_CONV =
   sig
      include PRIM_INTWORD_CONV

      val idFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int

      val zextdFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val zextdFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val zextdFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val zextdFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val zextdFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val zextdFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val zextdFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val zextdFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val zextdFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val zextdFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val zextdFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val zextdFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val zextdFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val zextdFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val zextdFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val zextdFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val zextdFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val sextdFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val sextdFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val sextdFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val sextdFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val sextdFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val sextdFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val sextdFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val sextdFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val sextdFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val sextdFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val sextdFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val sextdFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val sextdFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val sextdFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val sextdFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val sextdFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val sextdFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val castFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val castFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val castFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val castFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val castFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val castFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val castFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val castFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val castFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val castFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val castFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val castFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val castFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val castFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val castFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val castFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val castFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val zchckFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val zchckFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val zchckFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val zchckFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val zchckFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val zchckFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val zchckFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val zchckFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val zchckFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val zchckFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val zchckFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val zchckFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val zchckFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val zchckFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val zchckFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val zchckFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val zchckFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val schckFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val schckFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val schckFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val schckFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val schckFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val schckFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val schckFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val schckFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val schckFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val schckFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val schckFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val schckFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val schckFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val schckFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val schckFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val schckFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val schckFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word
   end
signature PRIM_INTEGER =
   sig
      include PRIM_INTEGER

      val zextdFromIntInf: Primitive.IntInf.int -> int
      val zextdToIntInf: int -> Primitive.IntInf.int

      val sextdFromIntInf: Primitive.IntInf.int -> int
      val sextdToIntInf: int -> Primitive.IntInf.int

      val castFromIntInf: Primitive.IntInf.int -> int
      val castToIntInf: int -> Primitive.IntInf.int

      val zchckFromIntInf: Primitive.IntInf.int -> int
      val zchckToIntInf: int -> Primitive.IntInf.int

      val schckFromIntInf: Primitive.IntInf.int -> int
      val schckToIntInf: int -> Primitive.IntInf.int
   end
signature PRIM_WORD =
   sig
      include PRIM_WORD

      val zextdFromIntInf: Primitive.IntInf.int -> word
      val zextdToIntInf: word -> Primitive.IntInf.int

      val sextdFromIntInf: Primitive.IntInf.int -> word
      val sextdToIntInf: word -> Primitive.IntInf.int

      val castFromIntInf: Primitive.IntInf.int -> word
      val castToIntInf: word -> Primitive.IntInf.int

      val zchckFromIntInf: Primitive.IntInf.int -> word
      val zchckToIntInf: word -> Primitive.IntInf.int

      val schckFromIntInf: Primitive.IntInf.int -> word
      val schckToIntInf: word -> Primitive.IntInf.int
   end

structure Primitive = struct

open Primitive

structure IntInf =
   struct
      structure Prim = Primitive.IntInf
      structure MLton = Primitive.MLton

      structure A = Primitive.Array
      structure V = Primitive.Vector
      structure S = SeqIndex
      structure W = struct
                       open ObjptrWord
                       local
                          structure S =
                             ObjptrInt_ChooseIntN
                             (type 'a t = 'a -> ObjptrWord.word
                              val fInt8 = ObjptrWord.zextdFromInt8
                              val fInt16 = ObjptrWord.zextdFromInt16
                              val fInt32 = ObjptrWord.zextdFromInt32
                              val fInt64 = ObjptrWord.zextdFromInt64)
                       in
                          val idFromObjptrInt = S.f
                       end
                       local
                          structure S =
                             ObjptrInt_ChooseIntN
                             (type 'a t = ObjptrWord.word -> 'a
                              val fInt8 = ObjptrWord.zextdToInt8
                              val fInt16 = ObjptrWord.zextdToInt16
                              val fInt32 = ObjptrWord.zextdToInt32
                              val fInt64 = ObjptrWord.zextdToInt64)
                       in
                          val idToObjptrInt = S.f
                       end
                    end
      structure I = ObjptrInt
      structure MPLimb = C_MPLimb
      structure Sz = struct 
                        open C_Size
                        local
                           structure S =
                              SeqIndex_ChooseIntN
                              (type 'a t = 'a -> C_Size.word
                               val fInt8 = C_Size.zextdFromInt8
                               val fInt16 = C_Size.zextdFromInt16
                               val fInt32 = C_Size.zextdFromInt32
                               val fInt64 = C_Size.zextdFromInt64)
                        in
                           val zextdFromSeqIndex = S.f
                        end
                     end

      type bigInt = Prim.int
      datatype rep =
         Big of MPLimb.t V.vector
       | Small of ObjptrInt.int

      val zero: bigInt = 0
      val one: bigInt = 1
      val negOne: bigInt = ~1

      (* Check if an IntInf.int is small (i.e., a fixnum). *)
      fun isSmall (i: bigInt): bool =
         0w0 <> W.andb (Prim.toWord i, 0w1)

      (* Check if two IntInf.int's are both small (i.e., fixnums). *)
      fun areSmall (i: bigInt, i': bigInt): bool =
         0w0 <> W.andb (W.andb (Prim.toWord i, Prim.toWord i'), 0w1)

      (* Return the number of `limbs' in a bigInt. *)
      fun bigNumLimbs i = S.- (V.length (Prim.toVector i), 1)
      fun numLimbs i = 
         if isSmall i
            then 1
            else bigNumLimbs i

      fun dropTag (w: W.word): W.word = W.~>>? (w, 0w1)
      fun dropTagCoerce (i: bigInt): W.word = dropTag (Prim.toWord i)
      fun dropTagCoerceInt (i: bigInt): I.int = W.idToObjptrInt (dropTagCoerce i)
      fun addTag (w: W.word): W.word = W.orb (W.<<? (w, 0w1), 0w1)
      fun addTagCoerce (w: W.word): bigInt = Prim.fromWord (addTag w)
      fun addTagCoerceInt (i: I.int): bigInt = addTagCoerce (W.idFromObjptrInt i)
      fun zeroTag (w: W.word): W.word = W.andb (w, W.notb 0w1)
      fun oneTag (w: W.word): W.word = W.orb (w, 0w1)
      fun oneTagCoerce (w: W.word): bigInt = Prim.fromWord (oneTag w)

      fun rep i =
         if isSmall i
            then Small (dropTagCoerceInt i)
            else Big (Prim.toVector i)

      local
         fun 'a make {zextdToMPLimb: 'a -> MPLimb.word,
                      zextdToObjptrWord: 'a -> ObjptrWord.word,
                      sextdToObjptrWord: 'a -> ObjptrWord.word,
                      other : {sizeInBits: Int32.int,
                               zero: 'a,
                               eq: 'a * 'a -> bool,
                               isNeg: 'a -> bool,
                               neg: 'a -> 'a,
                               notb: 'a -> 'a,
                               rashift: 'a * Word32.word -> 'a,
                               rshift: 'a * Word32.word -> 'a}}
                     (sextd, w) =
            if Int32.> (ObjptrWord.sizeInBits, #sizeInBits other)
               orelse let
                         val shift = Word32.- (ObjptrWord.sizeInBitsWord, 0w2)
                         val upperBits = (#rashift other) (w, shift)
                         val zeroBits = #zero other
                         val oneBits = (#notb other) zeroBits
                      in
                         (#eq other) (upperBits, zeroBits)
                         orelse
                         (sextd andalso (#eq other) (upperBits, oneBits))
                      end
               then if sextd
                       then Prim.fromWord (addTag (sextdToObjptrWord w))
                       else Prim.fromWord (addTag (zextdToObjptrWord w))
               else let
                       fun loop (w, i, acc) =
                          if (#eq other) (w, (#zero other))
                             then (i, acc)
                             else 
                                let
                                   val limb = zextdToMPLimb w
                                   val w = 
                                      (#rshift other) 
                                      (w, MPLimb.sizeInBitsWord)
                                in
                                   loop (w, S.+ (i, 1), (i, limb) :: acc)
                                end
                       val (n, acc) = 
                          if sextd andalso (#isNeg other) w
                             then loop ((#neg other) w, 1, [(0,0w1)])
                             else loop (w, 1, [(0,0w0)])
                       val a = A.arrayUnsafe n
                       fun loop acc =
                          case acc of
                             [] => ()
                           | (i, v) :: acc => (A.updateUnsafe (a, i, v)
                                               ; loop acc)
                       val () = loop acc
                    in
                       Prim.fromVector (V.fromArray a)
                    end
      in
         fun extdFromWord8 (sextd, w) =
            make {zextdToMPLimb = MPLimb.zextdFromWord8,
                  zextdToObjptrWord = ObjptrWord.zextdFromWord8,
                  sextdToObjptrWord = ObjptrWord.sextdFromWord8,
                  other = {sizeInBits = Word8.sizeInBits,
                           zero = Word8.zero,
                           eq = ((op =) : Word8.word * Word8.word -> bool),
                           isNeg = fn w => Int8.< (IntWordConv.idFromWord8ToInt8 w, 0),
                           neg = Word8.~,
                           notb = Word8.notb,
                           rashift = Word8.~>>?,
                           rshift = Word8.>>?}}
                 (sextd, w)
         fun zextdFromWord8 w = extdFromWord8 (false, w)
         fun zextdFromInt8 i = zextdFromWord8 (IntWordConv.idFromInt8ToWord8 i)
         fun sextdFromWord8 w = extdFromWord8 (true, w)
         fun sextdFromInt8 i = sextdFromWord8 (IntWordConv.idFromInt8ToWord8 i)
         val castFromInt8 = sextdFromInt8
         val castFromWord8 = zextdFromWord8
         val zchckFromInt8 = zextdFromInt8
         val zchckFromWord8 = zextdFromWord8
         val schckFromInt8 = sextdFromInt8
         val schckFromWord8 = sextdFromWord8

         fun extdFromWord16 (sextd, w) =
            make {zextdToMPLimb = MPLimb.zextdFromWord16,
                  zextdToObjptrWord = ObjptrWord.zextdFromWord16,
                  sextdToObjptrWord = ObjptrWord.sextdFromWord16,
                  other = {sizeInBits = Word16.sizeInBits,
                           zero = Word16.zero,
                           eq = ((op =) : Word16.word * Word16.word -> bool),
                           isNeg = fn w => Int16.< (IntWordConv.idFromWord16ToInt16 w, 0),
                           neg = Word16.~,
                           notb = Word16.notb,
                           rashift = Word16.~>>?,
                           rshift = Word16.>>?}}
                 (sextd, w)
         fun zextdFromWord16 w = extdFromWord16 (false, w)
         fun zextdFromInt16 i = zextdFromWord16 (IntWordConv.idFromInt16ToWord16 i)
         fun sextdFromWord16 w = extdFromWord16 (true, w)
         fun sextdFromInt16 i = sextdFromWord16 (IntWordConv.idFromInt16ToWord16 i)
         val castFromInt16 = sextdFromInt16
         val castFromWord16 = zextdFromWord16
         val zchckFromInt16 = zextdFromInt16
         val zchckFromWord16 = zextdFromWord16
         val schckFromInt16 = sextdFromInt16
         val schckFromWord16 = sextdFromWord16

         fun extdFromWord32 (sextd, w) =
            make {zextdToMPLimb = MPLimb.zextdFromWord32,
                  zextdToObjptrWord = ObjptrWord.zextdFromWord32,
                  sextdToObjptrWord = ObjptrWord.sextdFromWord32,
                  other = {sizeInBits = Word32.sizeInBits,
                           zero = Word32.zero,
                           eq = ((op =) : Word32.word * Word32.word -> bool),
                           isNeg = fn w => Int32.< (IntWordConv.idFromWord32ToInt32 w, 0),
                           neg = Word32.~,
                           notb = Word32.notb,
                           rashift = Word32.~>>?,
                           rshift = Word32.>>?}}
                 (sextd, w)
         fun zextdFromWord32 w = extdFromWord32 (false, w)
         fun zextdFromInt32 i = zextdFromWord32 (IntWordConv.idFromInt32ToWord32 i)
         fun sextdFromWord32 w = extdFromWord32 (true, w)
         fun sextdFromInt32 i = sextdFromWord32 (IntWordConv.idFromInt32ToWord32 i)
         val castFromInt32 = sextdFromInt32
         val castFromWord32 = zextdFromWord32
         val zchckFromInt32 = zextdFromInt32
         val zchckFromWord32 = zextdFromWord32
         val schckFromInt32 = sextdFromInt32
         val schckFromWord32 = sextdFromWord32

         fun extdFromWord64 (sextd, w) =
            make {zextdToMPLimb = MPLimb.zextdFromWord64,
                  zextdToObjptrWord = ObjptrWord.zextdFromWord64,
                  sextdToObjptrWord = ObjptrWord.sextdFromWord64,
                  other = {sizeInBits = Word64.sizeInBits,
                           zero = Word64.zero,
                           eq = ((op =) : Word64.word * Word64.word -> bool),
                           isNeg = fn w => Int64.< (IntWordConv.idFromWord64ToInt64 w, 0),
                           neg = Word64.~,
                           notb = Word64.notb,
                           rashift = Word64.~>>?,
                           rshift = Word64.>>?}}
                 (sextd, w)
         fun zextdFromWord64 w = extdFromWord64 (false, w)
         fun zextdFromInt64 i = zextdFromWord64 (IntWordConv.idFromInt64ToWord64 i)
         fun sextdFromWord64 w = extdFromWord64 (true, w)
         fun sextdFromInt64 i = sextdFromWord64 (IntWordConv.idFromInt64ToWord64 i)
         val castFromInt64 = sextdFromInt64
         val castFromWord64 = zextdFromWord64
         val zchckFromInt64 = zextdFromInt64
         val zchckFromWord64 = zextdFromWord64
         val schckFromInt64 = sextdFromInt64
         val schckFromWord64 = sextdFromWord64

         fun zextdFromIntInf ii = ii
         fun sextdFromIntInf ii = ii
         fun castFromIntInf ii = ii
         fun zchckFromIntInf ii = ii
         fun schckFromIntInf ii = ii
      end

      local
         structure S =
            ObjptrInt_ChooseIntN
            (type 'a t = 'a -> bigInt
             val fInt8 = sextdFromInt8
             val fInt16 = sextdFromInt16
             val fInt32 = sextdFromInt32
             val fInt64 = sextdFromInt64)
      in
         val sextdFromObjptrInt = S.f
      end

      local
         datatype 'a ans =
            Big of bool * bool * 'a
          | Small of ObjptrWord.word
         fun 'a make {zextdFromMPLimb: MPLimb.word -> 'a,
                      other : {sizeInBits: Int32.int,
                               sizeInBitsWord: Word32.word,
                               zero: 'a,
                               lshift: 'a * Word32.word -> 'a,
                               orb: 'a * 'a -> 'a}} i =
            if isSmall i
               then Small (dropTagCoerce i)
               else let
                       val v = Prim.toVector i
                       val n = V.length v
                       val isneg = V.subUnsafe (v, 0) <> 0w0
                    in
                       if Int32.>= (MPLimb.sizeInBits, #sizeInBits other) 
                          then let
                                  val limbsPer = 1
                                  val limb = V.subUnsafe (v, 1)
                                  val extra =
                                     S.> (n, S.+ (limbsPer, 1))
                                     orelse
                                     (MPLimb.>>? (limb, #sizeInBitsWord other)) <> 0w0
                                  val ans = zextdFromMPLimb limb
                               in
                                  Big (isneg, extra, ans)
                               end
                          else let
                                  val limbsPer =
                                     S.sextdFromInt32
                                     (Int32.quot (#sizeInBits other,
                                                  MPLimb.sizeInBits))
                                  val extra = S.> (n, S.+ (limbsPer, 1))
                                  val ans =
                                     let
                                        fun loop (i, ans) =
                                           if S.> (i, 0)
                                              then let
                                                      val limb = V.subUnsafe (v, i)
                                                      val ans = 
                                                         (#orb other) 
                                                         ((#lshift other) 
                                                          (ans, MPLimb.sizeInBitsWord),
                                                          zextdFromMPLimb limb)
                                                   in
                                                      loop (S.- (i, 1), ans)
                                                   end
                                              else ans
                                     in
                                        loop (S.min (S.- (n, 1), limbsPer), #zero other)
                                     end
                               in
                                  Big (isneg, extra, ans)
                               end
                    end
      in
         val chckToWord8Aux =
            make {zextdFromMPLimb = MPLimb.zextdToWord8,
                  other = {sizeInBits = Word8.sizeInBits,
                           sizeInBitsWord = Word8.sizeInBitsWord,
                           zero = Word8.zero,
                           lshift = Word8.<<?,
                           orb = Word8.orb}}
         fun sextdToWord8 i =
            case chckToWord8Aux i of
               Small w => ObjptrWord.sextdToWord8 w
             | Big (isneg, _, ans) => if isneg then Word8.~ ans else ans
         fun sextdToInt8 i = IntWordConv.idFromWord8ToInt8 (sextdToWord8 i)
         val zextdToWord8 = sextdToWord8
         fun zextdToInt8 i = IntWordConv.idFromWord8ToInt8 (zextdToWord8 i)
         val castToWord8 = sextdToWord8
         val castToInt8 = sextdToInt8
         fun schckToWord8 i =
            if not Primitive.Controls.detectOverflow
               then sextdToWord8 i
            else
            case chckToWord8Aux i of
               Small w => ObjptrWord.schckToWord8 w
             | Big (isneg, extra, ans) => 
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word8.~ ans
                             val ans' = IntWordConv.idFromWord8ToInt8 ans
                          in 
                             if Int8.> (ans', 0)
                                then raise Overflow
                                else ans
                          end
                  else let
                          val ans' = IntWordConv.idFromWord8ToInt8 ans
                       in
                          if Int8.< (ans', 0)
                             then raise Overflow
                             else ans
                       end
         fun schckToInt8 i = IntWordConv.idFromWord8ToInt8 (schckToWord8 i)
         fun zchckToWord8 i =
            if not Primitive.Controls.detectOverflow
               then zextdToWord8 i
            else
            case chckToWord8Aux i of
               Small w => ObjptrWord.schckToWord8 w
             | Big (isneg, extra, ans) => 
                  if isneg orelse extra
                     then raise Overflow
                  else ans
         fun zchckToInt8 i = IntWordConv.idFromWord8ToInt8 (zchckToWord8 i)

         val chckToWord16Aux =
            make {zextdFromMPLimb = MPLimb.zextdToWord16,
                  other = {sizeInBits = Word16.sizeInBits,
                           sizeInBitsWord = Word16.sizeInBitsWord,
                           zero = Word16.zero,
                           lshift = Word16.<<?,
                           orb = Word16.orb}}
         fun sextdToWord16 i =
            case chckToWord16Aux i of
               Small w => ObjptrWord.sextdToWord16 w
             | Big (isneg, _, ans) => if isneg then Word16.~ ans else ans
         fun sextdToInt16 i = IntWordConv.idFromWord16ToInt16 (sextdToWord16 i)
         val zextdToWord16 = sextdToWord16
         fun zextdToInt16 i = IntWordConv.idFromWord16ToInt16 (zextdToWord16 i)
         val castToWord16 = sextdToWord16
         val castToInt16 = sextdToInt16
         fun schckToWord16 i =
            if not Primitive.Controls.detectOverflow
               then sextdToWord16 i
            else
            case chckToWord16Aux i of
               Small w => ObjptrWord.schckToWord16 w
             | Big (isneg, extra, ans) => 
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word16.~ ans
                             val ans' = IntWordConv.idFromWord16ToInt16 ans
                          in 
                             if Int16.> (ans', 0)
                                then raise Overflow
                                else ans
                          end
                  else let
                          val ans' = IntWordConv.idFromWord16ToInt16 ans
                       in
                          if Int16.< (ans', 0)
                             then raise Overflow
                             else ans
                       end
         fun schckToInt16 i = IntWordConv.idFromWord16ToInt16 (schckToWord16 i)
         fun zchckToWord16 i =
            if not Primitive.Controls.detectOverflow
               then zextdToWord16 i
            else
            case chckToWord16Aux i of
               Small w => ObjptrWord.schckToWord16 w
             | Big (isneg, extra, ans) => 
                  if isneg orelse extra
                     then raise Overflow
                  else ans
         fun zchckToInt16 i = IntWordConv.idFromWord16ToInt16 (zchckToWord16 i)

         val chckToWord32Aux =
            make {zextdFromMPLimb = MPLimb.zextdToWord32,
                  other = {sizeInBits = Word32.sizeInBits,
                           sizeInBitsWord = Word32.sizeInBitsWord,
                           zero = Word32.zero,
                           lshift = Word32.<<?,
                           orb = Word32.orb}}
         fun sextdToWord32 i =
            case chckToWord32Aux i of
               Small w => ObjptrWord.sextdToWord32 w
             | Big (isneg, _, ans) => if isneg then Word32.~ ans else ans
         fun sextdToInt32 i = IntWordConv.idFromWord32ToInt32 (sextdToWord32 i)
         val zextdToWord32 = sextdToWord32
         fun zextdToInt32 i = IntWordConv.idFromWord32ToInt32 (zextdToWord32 i)
         val castToWord32 = sextdToWord32
         val castToInt32 = sextdToInt32
         fun schckToWord32 i =
            if not Primitive.Controls.detectOverflow
               then sextdToWord32 i
            else
            case chckToWord32Aux i of
               Small w => ObjptrWord.schckToWord32 w
             | Big (isneg, extra, ans) => 
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word32.~ ans
                             val ans' = IntWordConv.idFromWord32ToInt32 ans
                          in 
                             if Int32.> (ans', 0)
                                then raise Overflow
                                else ans
                          end
                  else let
                          val ans' = IntWordConv.idFromWord32ToInt32 ans
                       in
                          if Int32.< (ans', 0)
                             then raise Overflow
                             else ans
                       end
         fun schckToInt32 i = IntWordConv.idFromWord32ToInt32 (schckToWord32 i)
         fun zchckToWord32 i =
            if not Primitive.Controls.detectOverflow
               then zextdToWord32 i
            else
            case chckToWord32Aux i of
               Small w => ObjptrWord.schckToWord32 w
             | Big (isneg, extra, ans) => 
                  if isneg orelse extra
                     then raise Overflow
                  else ans
         fun zchckToInt32 i = IntWordConv.idFromWord32ToInt32 (zchckToWord32 i)

         val chckToWord64Aux =
            make {zextdFromMPLimb = MPLimb.zextdToWord64,
                  other = {sizeInBits = Word64.sizeInBits,
                           sizeInBitsWord = Word64.sizeInBitsWord,
                           zero = Word64.zero,
                           lshift = Word64.<<?,
                           orb = Word64.orb}}
         fun sextdToWord64 i =
            case chckToWord64Aux i of
               Small w => ObjptrWord.sextdToWord64 w
             | Big (isneg, _, ans) => if isneg then Word64.~ ans else ans
         fun sextdToInt64 i = IntWordConv.idFromWord64ToInt64 (sextdToWord64 i)
         val zextdToWord64 = sextdToWord64
         fun zextdToInt64 i = IntWordConv.idFromWord64ToInt64 (zextdToWord64 i)
         val castToWord64 = sextdToWord64
         val castToInt64 = sextdToInt64
         fun schckToWord64 i =
            if not Primitive.Controls.detectOverflow
               then sextdToWord64 i
            else
            case chckToWord64Aux i of
               Small w => ObjptrWord.schckToWord64 w
             | Big (isneg, extra, ans) => 
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word64.~ ans
                             val ans' = IntWordConv.idFromWord64ToInt64 ans
                          in 
                             if Int64.> (ans', 0)
                                then raise Overflow
                                else ans
                          end
                  else let
                          val ans' = IntWordConv.idFromWord64ToInt64 ans
                       in
                          if Int64.< (ans', 0)
                             then raise Overflow
                             else ans
                       end
         fun schckToInt64 i = IntWordConv.idFromWord64ToInt64 (schckToWord64 i)
         fun zchckToWord64 i =
            if not Primitive.Controls.detectOverflow
               then zextdToWord64 i
            else
            case chckToWord64Aux i of
               Small w => ObjptrWord.schckToWord64 w
             | Big (isneg, extra, ans) => 
                  if isneg orelse extra
                     then raise Overflow
                  else ans
         fun zchckToInt64 i = IntWordConv.idFromWord64ToInt64 (zchckToWord64 i)

         fun zextdToIntInf ii = ii
         fun sextdToIntInf ii = ii
         fun castToIntInf ii = ii
         fun zchckToIntInf ii = ii
         fun schckToIntInf ii = ii
      end

      local
         val bytesPerMPLimb = Sz.zextdFromInt32 (Int32.quot (MPLimb.sizeInBits, 8))
         val bytesPerCounter = Sz.zextdFromInt32 (Int32.quot (S.sizeInBits, 8))
         val bytesPerLength = Sz.zextdFromInt32 (Int32.quot (S.sizeInBits, 8))
         val bytesPerHeader = Sz.zextdFromInt32 (Int32.quot (HeaderWord.sizeInBits, 8))
      in
         val bytesPerArrayHeader =
            Sz.+ (bytesPerCounter, 
            Sz.+ (bytesPerLength, 
                  bytesPerHeader
            ))
         (* Reserve heap space for a large IntInf.int with room for num + extra
          * `limbs'.  The reason for splitting this up is that extra is intended
          * to be a constant, and so can be combined at compile time.
          *)
         fun reserve (num: S.int, extra: S.int) =
            Sz.+ (Sz.* (bytesPerMPLimb, Sz.zextdFromSeqIndex num),
            Sz.+ (Sz.* (bytesPerMPLimb, Sz.zextdFromSeqIndex extra),
            Sz.+ (bytesPerMPLimb, (* isneg Field *)
            Sz.+ (bytesPerArrayHeader, (* Array Header *)
                  case MLton.Align.align of (* alignment *)
                     MLton.Align.Align4 => 0w3
                   | MLton.Align.Align8 => 0w7
            ))))
      end

      (* badObjptr{Int,Word}{,Tagged} is the fixnum IntInf.int whose 
       * negation and absolute values are not fixnums. 
       * negBadIntInf is the negation (and absolute value) of that IntInf.int.
       *)
      val badObjptrInt: I.int = I.~>>? (I.minInt', 0w1)
      val badObjptrWord: W.word = W.idFromObjptrInt badObjptrInt
      val badObjptrWordTagged: W.word = addTag badObjptrWord
      (* val badObjptrIntTagged: I.int = W.idToObjptrInt badObjptrWordTagged *)
      val negBadIntInf: bigInt = sextdFromObjptrInt (I.~ badObjptrInt)

      (* Given two ObjptrWord.word's, check if they have the same 'high'/'sign' bit.
       *)
      fun sameSignBit (lhs: W.word, rhs: W.word): bool =
         I.>= (W.idToObjptrInt (W.xorb (lhs, rhs)), 0)

      (* Given a bignum bigint, test if it is (strictly) negative.
       *)
      fun bigIsNeg (arg: bigInt): bool =
         V.subUnsafe (Prim.toVector arg, 0) <> 0w0

      local
         fun make (smallOp, bigOp, limbsFn, extra)
                  (lhs: bigInt, rhs: bigInt): bigInt =
            let
               val res =
                  if areSmall (lhs, rhs)
                     then let
                             val lhsw = dropTagCoerce lhs
                             val lhsi = W.idToObjptrInt lhsw
                             val rhsw = dropTagCoerce rhs
                             val rhsi = W.idToObjptrInt rhsw
                             val ansi = smallOp (lhsi, rhsi)
                             val answ = W.idFromObjptrInt ansi
                             val ans = addTag answ
                          in
                             if sameSignBit (ans, answ)
                                then SOME (Prim.fromWord ans)
                                else NONE
                          end handle Overflow => NONE
                     else NONE
            in
               case res of
                  NONE => bigOp (lhs, rhs, 
                                 reserve (limbsFn (numLimbs lhs, numLimbs rhs), extra))
                | SOME i => i
            end
      in
         val bigAdd = make (I.+!, Prim.+, S.max, 1)
         val bigSub = make (I.-!, Prim.-, S.max, 1)
         val bigMul = make (I.*!, Prim.*, S.+, 0)
      end

      fun bigNeg (arg: bigInt): bigInt =
         if isSmall arg
            then let 
                    val argw = Prim.toWord arg
                 in 
                    if argw = badObjptrWordTagged
                       then negBadIntInf
                       else Prim.fromWord (W.- (0w2, argw))
                 end 
            else Prim.~ (arg, reserve (numLimbs arg, 1))


      fun bigQuot (num: bigInt, den: bigInt): bigInt =
         if areSmall (num, den)
            then let
                    val numw = dropTagCoerce num
                    val numi = W.idToObjptrInt numw
                    val denw = dropTagCoerce den
                    val deni = W.idToObjptrInt denw
                 in
                    if numw = badObjptrWord 
                       andalso deni = ~1
                       then negBadIntInf
                       else let
                               val ansi = I.quot (numi, deni)
                               val answ = W.idFromObjptrInt ansi
                               val ans = addTag answ
                            in 
                               Prim.fromWord ans
                            end
                 end
            else let
                    val nlimbs = numLimbs num
                    val dlimbs = numLimbs den
                 in
                    if S.< (nlimbs, dlimbs)
                       then zero
                       else if den = zero
                               then raise Div
                               else Prim.quot (num, den, 
                                               reserve (S.- (nlimbs, dlimbs), 2))
                 end

      fun bigRem (num: bigInt, den: bigInt): bigInt =
         if areSmall (num, den)
            then let 
                    val numw = dropTagCoerce num
                    val numi = W.idToObjptrInt numw
                    val denw = dropTagCoerce den
                    val deni = W.idToObjptrInt denw
                    val ansi = I.rem (numi, deni)
                    val answ = W.idFromObjptrInt ansi
                    val ans = addTag answ
                 in 
                    Prim.fromWord ans
                 end
            else let 
                    val nlimbs = numLimbs num
                    val dlimbs = numLimbs den
                 in 
                    if S.< (nlimbs, dlimbs)
                       then num
                       else if den = zero
                               then raise Div
                               else Prim.rem (num, den, 
                                              reserve (dlimbs, 1))
                 end

      (* Based on code from PolySpace. *)
      local
         open I

         fun mod2 x = I.andb (x, 1)
         fun div2 x = I.>>? (x, 0w1)

         fun smallGcd (a, b, acc) =
            case (a, b) of
               (0, _) => b * acc
             | (_, 0) => a * acc
             | (_, 1) => acc
             | (1, _) => acc
             | _ => 
                  if a = b
                     then a * acc
                     else
                        let
                           val a_2 = div2 a
                           val a_r2 = mod2 a
                           val b_2 = div2 b
                           val b_r2 = mod2 b
                        in
                           if 0 = a_r2
                              then
                                 if 0 = b_r2
                                    then smallGcd (a_2, b_2, acc + acc)
                                    else smallGcd (a_2, b, acc)
                              else
                                 if 0 = b_r2
                                    then smallGcd (a, b_2, acc)
                                    else
                                       if a >= b
                                          then smallGcd (div2 (a - b), b, acc)
                                          else smallGcd (a, div2 (b - a), acc)
                        end
      in
         fun bigGcd (lhs: bigInt, rhs: bigInt): bigInt =
            if areSmall (lhs, rhs)
               then addTagCoerceInt 
                    (smallGcd (I.abs (dropTagCoerceInt lhs),
                               I.abs (dropTagCoerceInt rhs),
                               1))
               else Prim.gcd 
                    (lhs, rhs, reserve (S.max (numLimbs lhs, numLimbs rhs), 0))
      end

      fun bigCompare (lhs: bigInt, rhs: bigInt): order =
         if areSmall (lhs, rhs)
            then I.compare (W.idToObjptrInt (Prim.toWord lhs),
                            W.idToObjptrInt (Prim.toWord rhs))
            else Int32.compare (Prim.compare (lhs, rhs), 0)

      local
         fun make (smallTest, int32Test)
                  (lhs: bigInt, rhs: bigInt): bool =
            if areSmall (lhs, rhs)
               then smallTest (W.idToObjptrInt (Prim.toWord lhs),
                               W.idToObjptrInt (Prim.toWord rhs))
               else int32Test (Prim.compare (lhs, rhs), 0)
      in
         val bigLT = make (I.<, Int32.<)
         val bigLE = make (I.<=, Int32.<=)
         val bigGT = make (I.>, Int32.>)
         val bigGE = make (I.>=, Int32.>=)
      end

      fun bigAbs (arg: bigInt): bigInt =
         if isSmall arg
            then let 
                    val argw = Prim.toWord arg
                 in 
                    if argw = badObjptrWordTagged
                       then negBadIntInf
                       else if I.< (W.idToObjptrInt argw, 0)
                               then Prim.fromWord (W.- (0w2, argw))
                               else arg
                 end
            else if bigIsNeg arg
                    then Prim.~ (arg, reserve (numLimbs arg, 1))
                    else arg

      fun bigMin (lhs: bigInt, rhs: bigInt): bigInt =
         if bigLE (lhs, rhs) then lhs else rhs

      fun bigMax (lhs: bigInt, rhs: bigInt): bigInt =
         if bigLE (lhs, rhs) then rhs else lhs

      local
         fun bigLTU (lhs, rhs) =
            case (bigCompare (lhs, 0), bigCompare (rhs, 0)) of
               (LESS, LESS) => bigLT (lhs, rhs)
             | (LESS, GREATER) => false
             | (_, EQUAL) => false
             | (EQUAL, _) => true
             | (GREATER, LESS) => true
             | (GREATER, GREATER) => bigLT (lhs, rhs)
         structure S = IntegralComparisons(type t = bigInt
                                           val op < = bigLTU)
      in
         val bigLTU = S.<
         val bigLEU = S.<=
         val bigGTU = S.>
         val bigGEU = S.>=
      end

      local
         val op + = bigAdd
         val op - = bigSub
         val op > = bigGT
         val op >= = bigGE
         val op < = bigLT
         val quot = bigQuot
         val rem = bigRem
      in
         fun bigDiv (x, y) =
            if x >= zero
               then if y > zero
                       then quot (x, y)
                       else if y < zero
                               then if x = zero
                                       then zero
                                       else quot (x - one, y) - one
                               else raise Div
               else if y < zero
                       then quot (x, y)
                       else if y > zero
                               then quot (x + one, y) - one
                               else raise Div

         fun bigMod (x, y) =
            if x >= zero
               then if y > zero
                       then rem (x, y)
                       else if y < zero
                               then if x = zero
                                       then zero
                                       else rem (x - one, y) + (one + y)
                               else raise Div
               else if y < zero
                       then rem (x, y)
                       else if y > zero
                               then rem (x + one, y) + (y - one)
                               else raise Div

         fun bigDivMod (x, y) = (bigDiv (x, y), bigMod (x, y))
         fun bigQuotRem (x, y) = (bigQuot (x, y), bigRem (x, y))
      end

      local
         fun make (smallOp, bigOp) 
                  (lhs: bigInt, rhs: bigInt) =
            if areSmall (lhs, rhs)
               then
                  let
                     val answ = smallOp (Prim.toWord lhs, Prim.toWord rhs)
                     val ans = oneTagCoerce answ
                  in
                     ans
                  end
               else bigOp (lhs, rhs, 
                           reserve (S.max (numLimbs lhs, numLimbs rhs), 0))
      in
         val bigAndb = make (W.andb, Prim.andb)
         val bigOrb = make (W.orb, Prim.orb)
         val bigXorb = make (W.xorb, Prim.xorb)
      end

      fun bigNotb (arg: bigInt): bigInt =
         if isSmall arg
            then oneTagCoerce (W.notb (Prim.toWord arg))
            else Prim.notb (arg, reserve (numLimbs arg, 0))

      local
         val bitsPerLimb = MPLimb.sizeInBitsWord
         fun shiftSize shift = S.sextdFromWord32 (Word32.div (shift, bitsPerLimb))
      in
         fun bigLshift (arg: bigInt, shift: Word32.word): bigInt =
            if shift = 0wx0
               then arg
               else Prim.<< (arg, shift, 
                             reserve (S.+ (numLimbs arg, shiftSize shift), 1))
         fun bigRashift (arg: bigInt, shift: Word32.word): bigInt =
            if shift = 0wx0
               then arg
               else Prim.~>> (arg, shift,
                              reserve (S.max (1, S.- (numLimbs arg, shiftSize shift)), 0))
      end

      fun mkBigCvt {base: Int32.int,
                    smallCvt: I.int -> Primitive.String8.string}
                   (arg: bigInt)
                   : Primitive.String8.string =
         if isSmall arg
            then smallCvt (dropTagCoerceInt arg)
            else let
                    val bpd = Int32.log2 base
                    val bpl = MPLimb.sizeInBits
                    val dpl =
                       Int32.+ (Int32.quot (bpl, bpd),
                                if Int32.mod (bpl, bpd) = 0
                                   then 0 else 1)
                    val bytes =
                       Sz.+ (Sz.+ (bytesPerArrayHeader (* Array Header *),
                             Sz.+ (0w1 (* sign *),
                                   case MLton.Align.align of (* alignment *)
                                      MLton.Align.Align4 => 0w3
                                    | MLton.Align.Align8 => 0w7)),
                             Sz.* (Sz.zextdFromInt32 dpl, 
                                   Sz.zextdFromSeqIndex (numLimbs arg)))
                 in
                    Prim.toString (arg, base, bytes)
                 end

      fun mkBigLog2 {fromSmall: {smallLog2: Primitive.Int32.int} -> 'a,
                     fromLarge: {numLimbsMinusOne: SeqIndex.int,
                                 mostSigLimbLog2: Primitive.Int32.int} -> 'a}
                    (arg: bigInt) =
         if bigLE (arg, 0)
            then raise Domain
            else if isSmall arg
                    then fromSmall {smallLog2 = W.log2 (dropTagCoerce arg)}
                    else let
                            val v = Prim.toVector arg
                            val n = V.length v
                            val w = MPLimb.log2 (V.subUnsafe (v, S.- (n, 1)))
                         in
                            fromLarge {numLimbsMinusOne = S.- (n, 2),
                                       mostSigLimbLog2 = w}
                         end

      type int = bigInt
      type t = int

      val precision = NONE

      val maxInt = NONE
      val minInt = NONE

      structure Prim = 
         struct
            val isSmall = isSmall
            val areSmall = areSmall
            val dropTag = dropTag
            val dropTagCoerce = dropTagCoerce
            val dropTagCoerceInt = dropTagCoerceInt
            val addTag = addTag
            val addTagCoerce = addTagCoerce
            val addTagCoerceInt = addTagCoerceInt
            val zeroTag = zeroTag
            val oneTag = oneTag
            val oneTagCoerce = oneTagCoerce
        end

      val abs = bigAbs
      val op +! = bigAdd
      val op +? = bigAdd
      val op + = bigAdd
      val divMod = bigDivMod
      val op div = bigDiv
      val gcd = bigGcd
      val op mod = bigMod
      val op *! = bigMul
      val op *? = bigMul
      val op * = bigMul
      val op ~! = bigNeg
      val op ~? = bigNeg
      val op ~ = bigNeg
      val quotRem = bigQuotRem
      val quot = bigQuot
      val rem = bigRem
      val op -! = bigSub
      val op -? = bigSub
      val op - = bigSub

      val op < = bigLT
      val op <= = bigLE
      val op > = bigGT
      val op >= = bigGE
      val compare = bigCompare
      val min = bigMin
      val max = bigMax
      val ltu = bigLTU
      val leu = bigLEU
      val gtu = bigGTU
      val geu = bigGEU
      val isNeg = bigIsNeg

      val andb = bigAndb
      val <<? = bigLshift
      val << = bigLshift
      val notb = bigNotb
      val orb = bigOrb
      val ~>>? = bigRashift
      val ~>> = bigRashift
      val xorb = bigXorb

      val mkCvt = mkBigCvt
      val mkLog2 = mkBigLog2
end

structure IntWordConv : PRIM_INTWORD_CONV =
   struct
      open IntWordConv

      val idFromIntInfToIntInf = fn i => i


      val zextdFromInt8ToIntInf = IntInf.zextdFromInt8
      val zextdFromInt16ToIntInf = IntInf.zextdFromInt16
      val zextdFromInt32ToIntInf = IntInf.zextdFromInt32
      val zextdFromInt64ToIntInf = IntInf.zextdFromInt64
      val zextdFromWord8ToIntInf = IntInf.zextdFromWord8
      val zextdFromWord16ToIntInf = IntInf.zextdFromWord16
      val zextdFromWord32ToIntInf = IntInf.zextdFromWord32
      val zextdFromWord64ToIntInf = IntInf.zextdFromWord64

      val zextdFromIntInfToInt8 = IntInf.zextdToInt8
      val zextdFromIntInfToInt16 = IntInf.zextdToInt16
      val zextdFromIntInfToInt32 = IntInf.zextdToInt32
      val zextdFromIntInfToInt64 = IntInf.zextdToInt64
      val zextdFromIntInfToIntInf = IntInf.zextdToIntInf
      val zextdFromIntInfToWord8 = IntInf.zextdToWord8
      val zextdFromIntInfToWord16 = IntInf.zextdToWord16
      val zextdFromIntInfToWord32 = IntInf.zextdToWord32
      val zextdFromIntInfToWord64 = IntInf.zextdToWord64


      val sextdFromInt8ToIntInf = IntInf.sextdFromInt8
      val sextdFromInt16ToIntInf = IntInf.sextdFromInt16
      val sextdFromInt32ToIntInf = IntInf.sextdFromInt32
      val sextdFromInt64ToIntInf = IntInf.sextdFromInt64
      val sextdFromWord8ToIntInf = IntInf.sextdFromWord8
      val sextdFromWord16ToIntInf = IntInf.sextdFromWord16
      val sextdFromWord32ToIntInf = IntInf.sextdFromWord32
      val sextdFromWord64ToIntInf = IntInf.sextdFromWord64

      val sextdFromIntInfToInt8 = IntInf.sextdToInt8
      val sextdFromIntInfToInt16 = IntInf.sextdToInt16
      val sextdFromIntInfToInt32 = IntInf.sextdToInt32
      val sextdFromIntInfToInt64 = IntInf.sextdToInt64
      val sextdFromIntInfToIntInf = IntInf.sextdToIntInf
      val sextdFromIntInfToWord8 = IntInf.sextdToWord8
      val sextdFromIntInfToWord16 = IntInf.sextdToWord16
      val sextdFromIntInfToWord32 = IntInf.sextdToWord32
      val sextdFromIntInfToWord64 = IntInf.sextdToWord64


      val castFromInt8ToIntInf = IntInf.castFromInt8
      val castFromInt16ToIntInf = IntInf.castFromInt16
      val castFromInt32ToIntInf = IntInf.castFromInt32
      val castFromInt64ToIntInf = IntInf.castFromInt64
      val castFromWord8ToIntInf = IntInf.castFromWord8
      val castFromWord16ToIntInf = IntInf.castFromWord16
      val castFromWord32ToIntInf = IntInf.castFromWord32
      val castFromWord64ToIntInf = IntInf.castFromWord64

      val castFromIntInfToInt8 = IntInf.castToInt8
      val castFromIntInfToInt16 = IntInf.castToInt16
      val castFromIntInfToInt32 = IntInf.castToInt32
      val castFromIntInfToInt64 = IntInf.castToInt64
      val castFromIntInfToIntInf = IntInf.castToIntInf
      val castFromIntInfToWord8 = IntInf.castToWord8
      val castFromIntInfToWord16 = IntInf.castToWord16
      val castFromIntInfToWord32 = IntInf.castToWord32
      val castFromIntInfToWord64 = IntInf.castToWord64


      val zchckFromInt8ToIntInf = IntInf.zchckFromInt8
      val zchckFromInt16ToIntInf = IntInf.zchckFromInt16
      val zchckFromInt32ToIntInf = IntInf.zchckFromInt32
      val zchckFromInt64ToIntInf = IntInf.zchckFromInt64
      val zchckFromWord8ToIntInf = IntInf.zchckFromWord8
      val zchckFromWord16ToIntInf = IntInf.zchckFromWord16
      val zchckFromWord32ToIntInf = IntInf.zchckFromWord32
      val zchckFromWord64ToIntInf = IntInf.zchckFromWord64

      val zchckFromIntInfToInt8 = IntInf.zchckToInt8
      val zchckFromIntInfToInt16 = IntInf.zchckToInt16
      val zchckFromIntInfToInt32 = IntInf.zchckToInt32
      val zchckFromIntInfToInt64 = IntInf.zchckToInt64
      val zchckFromIntInfToIntInf = IntInf.zchckToIntInf
      val zchckFromIntInfToWord8 = IntInf.zchckToWord8
      val zchckFromIntInfToWord16 = IntInf.zchckToWord16
      val zchckFromIntInfToWord32 = IntInf.zchckToWord32
      val zchckFromIntInfToWord64 = IntInf.zchckToWord64


      val schckFromInt8ToIntInf = IntInf.schckFromInt8
      val schckFromInt16ToIntInf = IntInf.schckFromInt16
      val schckFromInt32ToIntInf = IntInf.schckFromInt32
      val schckFromInt64ToIntInf = IntInf.schckFromInt64
      val schckFromWord8ToIntInf = IntInf.schckFromWord8
      val schckFromWord16ToIntInf = IntInf.schckFromWord16
      val schckFromWord32ToIntInf = IntInf.schckFromWord32
      val schckFromWord64ToIntInf = IntInf.schckFromWord64

      val schckFromIntInfToInt8 = IntInf.schckToInt8
      val schckFromIntInfToInt16 = IntInf.schckToInt16
      val schckFromIntInfToInt32 = IntInf.schckToInt32
      val schckFromIntInfToInt64 = IntInf.schckToInt64
      val schckFromIntInfToIntInf = IntInf.schckToIntInf
      val schckFromIntInfToWord8 = IntInf.schckToWord8
      val schckFromIntInfToWord16 = IntInf.schckToWord16
      val schckFromIntInfToWord32 = IntInf.schckToWord32
      val schckFromIntInfToWord64 = IntInf.schckToWord64
   end

structure Int8 : PRIM_INTEGER =
   struct
      open Int8

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToInt8
      val zextdToIntInf = IntWordConv.zextdFromInt8ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToInt8
      val sextdToIntInf = IntWordConv.sextdFromInt8ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToInt8
      val castToIntInf = IntWordConv.castFromInt8ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToInt8
      val zchckToIntInf = IntWordConv.zchckFromInt8ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToInt8
      val schckToIntInf = IntWordConv.schckFromInt8ToIntInf
   end
structure Int16 : PRIM_INTEGER =
   struct
      open Int16

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToInt16
      val zextdToIntInf = IntWordConv.zextdFromInt16ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToInt16
      val sextdToIntInf = IntWordConv.sextdFromInt16ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToInt16
      val castToIntInf = IntWordConv.castFromInt16ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToInt16
      val zchckToIntInf = IntWordConv.zchckFromInt16ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToInt16
      val schckToIntInf = IntWordConv.schckFromInt16ToIntInf
   end
structure Int32 : PRIM_INTEGER =
   struct
      open Int32

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToInt32
      val zextdToIntInf = IntWordConv.zextdFromInt32ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToInt32
      val sextdToIntInf = IntWordConv.sextdFromInt32ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToInt32
      val castToIntInf = IntWordConv.castFromInt32ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToInt32
      val zchckToIntInf = IntWordConv.zchckFromInt32ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToInt32
      val schckToIntInf = IntWordConv.schckFromInt32ToIntInf
   end
structure Int64 : PRIM_INTEGER =
   struct
      open Int64

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToInt64
      val zextdToIntInf = IntWordConv.zextdFromInt64ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToInt64
      val sextdToIntInf = IntWordConv.sextdFromInt64ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToInt64
      val castToIntInf = IntWordConv.castFromInt64ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToInt64
      val zchckToIntInf = IntWordConv.zchckFromInt64ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToInt64
      val schckToIntInf = IntWordConv.schckFromInt64ToIntInf
   end
structure Word8 : PRIM_WORD =
   struct
      open Word8

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToWord8
      val zextdToIntInf = IntWordConv.zextdFromWord8ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToWord8
      val sextdToIntInf = IntWordConv.sextdFromWord8ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToWord8
      val castToIntInf = IntWordConv.castFromWord8ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToWord8
      val zchckToIntInf = IntWordConv.zchckFromWord8ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToWord8
      val schckToIntInf = IntWordConv.schckFromWord8ToIntInf
   end
structure Word16 : PRIM_WORD =
   struct
      open Word16

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToWord16
      val zextdToIntInf = IntWordConv.zextdFromWord16ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToWord16
      val sextdToIntInf = IntWordConv.sextdFromWord16ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToWord16
      val castToIntInf = IntWordConv.castFromWord16ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToWord16
      val zchckToIntInf = IntWordConv.zchckFromWord16ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToWord16
      val schckToIntInf = IntWordConv.schckFromWord16ToIntInf
   end
structure Word32 : PRIM_WORD =
   struct
      open Word32

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToWord32
      val zextdToIntInf = IntWordConv.zextdFromWord32ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToWord32
      val sextdToIntInf = IntWordConv.sextdFromWord32ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToWord32
      val castToIntInf = IntWordConv.castFromWord32ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToWord32
      val zchckToIntInf = IntWordConv.zchckFromWord32ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToWord32
      val schckToIntInf = IntWordConv.schckFromWord32ToIntInf
   end
structure Word64 : PRIM_WORD =
   struct
      open Word64

      val zextdFromIntInf = IntWordConv.zextdFromIntInfToWord64
      val zextdToIntInf = IntWordConv.zextdFromWord64ToIntInf

      val sextdFromIntInf = IntWordConv.sextdFromIntInfToWord64
      val sextdToIntInf = IntWordConv.sextdFromWord64ToIntInf

      val castFromIntInf = IntWordConv.castFromIntInfToWord64
      val castToIntInf = IntWordConv.castFromWord64ToIntInf

      val zchckFromIntInf = IntWordConv.zchckFromIntInfToWord64
      val zchckToIntInf = IntWordConv.zchckFromWord64ToIntInf

      val schckFromIntInf = IntWordConv.schckFromIntInfToWord64
      val schckToIntInf = IntWordConv.schckFromWord64ToIntInf
   end

structure IntInf : PRIM_INT_INF = IntInf

end
