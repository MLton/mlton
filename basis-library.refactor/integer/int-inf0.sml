(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INT_INF0 =
   sig
      eqtype int
      type t = int

      datatype rep =
         Big of C_MPLimb.word vector
       | Small of ObjptrInt.int
      val rep: int -> rep

      val maxInt: int option
      val minInt: int option

      val zero: int
      val one: int
      val negOne: int

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
      val +? : int * int -> int
      val + : int * int -> int
      val divMod: int * int -> int * int
      val div: int * int -> int
      val gcd: int * int -> int
      val mod: int * int -> int
      val *? : int * int -> int
      val * : int * int -> int
      val ~? : int -> int
      val ~ : int -> int
      val quotRem: int * int -> int * int
      val quot: int * int -> int
      val rem: int * int -> int
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
      val << : int * Primitive.Word32.word -> int
      val notb: int -> int
      val orb: int * int -> int
      val ~>> : int * Primitive.Word32.word -> int
      val xorb: int * int -> int

      val mkCvt: ({base: Primitive.Int32.int,
                   smallCvt: ObjptrInt.int -> Primitive.String8.string} 
                  -> int -> Primitive.String8.string)
      val mkLog2: ({fromSmall: {smallLog2: Primitive.Int32.int} -> 'a,
                    fromLarge: {mostSigLimbLog2: Primitive.Int32.int,
                                numLimbsMinusOne: SeqIndex.int} -> 'a}
                   -> int -> 'a)

      (* Sign extend. *)
      val fromInt8Unsafe: Primitive.Int8.int -> int
      val fromInt16Unsafe: Primitive.Int16.int -> int
      val fromInt32Unsafe: Primitive.Int32.int -> int
      val fromInt64Unsafe: Primitive.Int64.int -> int
      val fromIntInfUnsafe: Primitive.IntInf.int -> int

      (* Sign extend. *)
      val fromInt8: Primitive.Int8.int -> int
      val fromInt16: Primitive.Int16.int -> int
      val fromInt32: Primitive.Int32.int -> int
      val fromInt64: Primitive.Int64.int -> int
      val fromIntInf: Primitive.IntInf.int -> int

      (* Zero extend. *)
      val fromWord8Unsafe: Primitive.Word8.word -> int
      val fromWord16Unsafe: Primitive.Word16.word -> int
      val fromWord32Unsafe: Primitive.Word32.word -> int
      val fromWord64Unsafe: Primitive.Word64.word -> int

      (* Zero extend. *)
      val fromWord8: Primitive.Word8.word -> int
      val fromWord16: Primitive.Word16.word -> int
      val fromWord32: Primitive.Word32.word -> int
      val fromWord64: Primitive.Word64.word -> int

      (* Sign extend. *)
      val fromWord8XUnsafe: Primitive.Word8.word -> int
      val fromWord16XUnsafe: Primitive.Word16.word -> int
      val fromWord32XUnsafe: Primitive.Word32.word -> int
      val fromWord64XUnsafe: Primitive.Word64.word -> int

      (* Sign extend. *)
      val fromWord8X: Primitive.Word8.word -> int
      val fromWord16X: Primitive.Word16.word -> int
      val fromWord32X: Primitive.Word32.word -> int
      val fromWord64X: Primitive.Word64.word -> int

      (* Lowbits. *)
      val toInt8Unsafe: int -> Primitive.Int8.int
      val toInt16Unsafe: int -> Primitive.Int16.int
      val toInt32Unsafe: int -> Primitive.Int32.int
      val toInt64Unsafe: int -> Primitive.Int64.int
      val toIntInfUnsafe: int -> Primitive.IntInf.int

      (* Overflow checking. *)
      val toInt8: int -> Primitive.Int8.int
      val toInt16: int -> Primitive.Int16.int
      val toInt32: int -> Primitive.Int32.int
      val toInt64: int -> Primitive.Int64.int
      val toIntInf: int -> Primitive.IntInf.int

      (* Lowbits. *)
      val toWord8Unsafe: int -> Primitive.Word8.word
      val toWord16Unsafe: int -> Primitive.Word16.word
      val toWord32Unsafe: int -> Primitive.Word32.word
      val toWord64Unsafe: int -> Primitive.Word64.word

      (* Lowbits. *)
      val toWord8: int -> Primitive.Word8.word
      val toWord16: int -> Primitive.Word16.word
      val toWord32: int -> Primitive.Word32.word
      val toWord64: int -> Primitive.Word64.word

      (* Lowbits. *)
      val toWord8XUnsafe: int -> Primitive.Word8.word
      val toWord16XUnsafe: int -> Primitive.Word16.word
      val toWord32XUnsafe: int -> Primitive.Word32.word
      val toWord64XUnsafe: int -> Primitive.Word64.word

      (* Lowbits. *)
      val toWord8X: int -> Primitive.Word8.word
      val toWord16X: int -> Primitive.Word16.word
      val toWord32X: int -> Primitive.Word32.word
      val toWord64X: int -> Primitive.Word64.word
   end

structure Primitive = struct

open Primitive

structure IntInf : INT_INF0 =
   struct
      structure Prim = Primitive.IntInf

      structure A = Primitive.Array
      structure V = Primitive.Vector
      structure S = SeqIndex
      structure W = struct
                       open ObjptrWord
                       local
                          structure S =
                             ObjptrInt_ChooseIntN
                             (type 'a t = 'a -> ObjptrWord.word
                              val fInt8 = ObjptrWord.fromInt8
                              val fInt16 = ObjptrWord.fromInt16
                              val fInt32 = ObjptrWord.fromInt32
                              val fInt64 = ObjptrWord.fromInt64)
                       in
                          val fromObjptrInt = S.f
                       end
                       local
                          structure S =
                             ObjptrInt_ChooseIntN
                             (type 'a t = ObjptrWord.word -> 'a
                              val fInt8 = ObjptrWord.toInt8
                              val fInt16 = ObjptrWord.toInt16
                              val fInt32 = ObjptrWord.toInt32
                              val fInt64 = ObjptrWord.toInt64)
                       in
                          val toObjptrInt = S.f
                       end
                       local
                          structure S =
                             ObjptrInt_ChooseIntN
                             (type 'a t = ObjptrWord.word -> 'a
                              val fInt8 = ObjptrWord.toInt8X
                              val fInt16 = ObjptrWord.toInt16X
                              val fInt32 = ObjptrWord.toInt32X
                              val fInt64 = ObjptrWord.toInt64X)
                       in
                          val toObjptrIntX = S.f
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
                               val fInt8 = C_Size.fromInt8
                               val fInt16 = C_Size.fromInt16
                               val fInt32 = C_Size.fromInt32
                               val fInt64 = C_Size.fromInt64)
                        in
                           val fromSeqIndex = S.f
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

      fun dropTag (w: W.word): W.word = W.~>> (w, 0w1)
      fun dropTagCoerce (i: bigInt): W.word = dropTag (Prim.toWord i)
      fun dropTagCoerceInt (i: bigInt): I.int = W.toObjptrIntX (dropTagCoerce i)
      fun addTag (w: W.word): W.word = W.orb (W.<< (w, 0w1), 0w1)
      fun addTagCoerce (w: W.word): bigInt = Prim.fromWord (addTag w)
      fun addTagCoerceInt (i: I.int): bigInt = addTagCoerce (W.fromObjptrInt i)
      fun zeroTag (w: W.word): W.word = W.andb (w, W.notb 0w1)
      fun oneTag (w: W.word): W.word = W.orb (w, 0w1)
      fun oneTagCoerce (w: W.word): bigInt = Prim.fromWord (oneTag w)

      fun rep i =
         if isSmall i
            then Small (dropTagCoerceInt i)
            else Big (Prim.toVector i)

      local
         fun 'a make {toMPLimb: 'a -> MPLimb.word,
                      toObjptrWord: 'a -> ObjptrWord.word,
                      other : {wordSize: Int32.int,
                               zero: 'a,
                               eq: 'a * 'a -> bool,
                               rshift: 'a * Word32.word -> 'a}} 
                     (isneg, w) =
            if Int32.> (ObjptrWord.wordSize, #wordSize other)
               orelse let
                         val upperBits =
                            (#rshift other)
                            (w, Word32.- (ObjptrWord.wordSizeWord, 0w2))
                      in
                         (#eq other) (upperBits, #zero other)
                      end
               then let
                       val ans = toObjptrWord w
                       val ans = if isneg then ObjptrWord.~ ans else ans
                    in 
                       Prim.fromWord (addTag ans)
                    end
               else let
                       fun loop (w, i, acc) =
                          if (#eq other) (w, (#zero other))
                             then (i, acc)
                             else 
                                let
                                   val limb = toMPLimb w
                                   val w = 
                                      (#rshift other) 
                                      (w, MPLimb.wordSizeWord)
                                in
                                   loop (w, S.+ (i, 1), (i, limb) :: acc)
                                end
                       val (n, acc) = 
                          loop (w, 1, [(0, if isneg then 0w1 else 0w0)])
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
         val fromWordAux8 =
            make {toMPLimb = MPLimb.fromWord8,
                  toObjptrWord = ObjptrWord.fromWord8,
                  other = {wordSize = Word8.wordSize,
                           zero = Word8.zero,
                           eq = ((op =) : Word8.word * Word8.word -> bool),
                           rshift = Word8.>>}}
         fun fromWord8 w = fromWordAux8 (false, w)
         fun fromInt8 i =
            if Int8.>= (i, 0)
               then fromWordAux8 (false, Word8.fromInt8 i)
               else fromWordAux8 (true, Word8.~ (Word8.fromInt8 i))
         fun fromWord8X w = fromInt8 (Word8.toInt8X w)
         val fromInt8Unsafe = fromInt8
         val fromWord8Unsafe = fromWord8
         val fromWord8XUnsafe = fromWord8X

         val fromWordAux16 =
            make {toMPLimb = MPLimb.fromWord16,
                  toObjptrWord = ObjptrWord.fromWord16,
                  other = {wordSize = Word16.wordSize,
                           zero = Word16.zero,
                           eq = ((op =) : Word16.word * Word16.word -> bool),
                           rshift = Word16.>>}}
         fun fromWord16 w = fromWordAux16 (false, w)
         fun fromInt16 i =
            if Int16.>= (i, 0)
               then fromWordAux16 (false, Word16.fromInt16 i)
               else fromWordAux16 (true, Word16.~ (Word16.fromInt16 i))
         fun fromWord16X w = fromInt16 (Word16.toInt16X w)
         val fromInt16Unsafe = fromInt16
         val fromWord16Unsafe = fromWord16
         val fromWord16XUnsafe = fromWord16X

         val fromWordAux32 =
            make {toMPLimb = MPLimb.fromWord32,
                  toObjptrWord = ObjptrWord.fromWord32,
                  other = {wordSize = Word32.wordSize,
                           zero = Word32.zero,
                           eq = ((op =) : Word32.word * Word32.word -> bool),
                           rshift = Word32.>>}}
         fun fromWord32 w = fromWordAux32 (false, w)
         fun fromInt32 i =
            if Int32.>= (i, 0)
               then fromWordAux32 (false, Word32.fromInt32 i)
               else fromWordAux32 (true, Word32.~ (Word32.fromInt32 i))
         fun fromWord32X w = fromInt32 (Word32.toInt32X w)
         val fromInt32Unsafe = fromInt32
         val fromWord32Unsafe = fromWord32
         val fromWord32XUnsafe = fromWord32X

         val fromWordAux64 =
            make {toMPLimb = MPLimb.fromWord64,
                  toObjptrWord = ObjptrWord.fromWord64,
                  other = {wordSize = Word64.wordSize,
                           zero = Word64.zero,
                           eq = ((op =) : Word64.word * Word64.word -> bool),
                           rshift = Word64.>>}}
         fun fromWord64 w = fromWordAux64 (false, w)
         fun fromInt64 i =
            if Int64.>= (i, 0)
               then fromWordAux64 (false, Word64.fromInt64 i)
               else fromWordAux64 (true, Word64.~ (Word64.fromInt64 i))
         fun fromWord64X w = fromInt64 (Word64.toInt64X w)
         val fromInt64Unsafe = fromInt64
         val fromWord64Unsafe = fromWord64
         val fromWord64XUnsafe = fromWord64X

         fun fromIntInf i = i
         fun fromIntInfUnsafe i = i
      end

      local
         structure S =
            ObjptrInt_ChooseIntN
            (type 'a t = 'a -> bigInt
             val fInt8 = fromInt8
             val fInt16 = fromInt16
             val fInt32 = fromInt32
             val fInt64 = fromInt64)
      in
         val fromObjptrInt = S.f
      end

      local
         datatype 'a ans =
            Big of bool * bool * 'a
          | Small of ObjptrWord.word
         fun 'a make {fromMPLimb: MPLimb.word -> 'a,
                      other : {wordSize: Int32.int,
                               wordSizeWord: Word32.word,
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
                       if Int32.>= (MPLimb.wordSize, #wordSize other) 
                          then let
                                  val limbsPer = 1
                                  val limb = V.subUnsafe (v, 1)
                                  val extra =
                                     S.> (n, S.+ (limbsPer, 1))
                                     orelse
                                     (MPLimb.>> (limb, #wordSizeWord other)) <> 0w0
                                  val ans = fromMPLimb limb
                               in
                                  Big (isneg, extra, ans)
                               end
                          else let
                                  val limbsPer =
                                     S.fromInt32 (Int32.quot (#wordSize other, 
                                                              MPLimb.wordSize))
                                  val extra =
                                     S.> (n, S.+ (limbsPer, 1))
                                  val ans =
                                     let
                                        fun loop (i, ans) =
                                           if S.> (i, 0)
                                              then let
                                                      val limb = V.subUnsafe (v, i)
                                                      val ans = 
                                                         (#orb other) 
                                                         ((#lshift other) 
                                                          (ans, MPLimb.wordSizeWord),
                                                          fromMPLimb limb)
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
         val toWordAux8 =
            make {fromMPLimb = MPLimb.toWord8,
                  other = {wordSize = Word8.wordSize,
                           wordSizeWord = Word8.wordSizeWord,
                           zero = Word8.zero,
                           lshift = Word8.<<,
                           orb = Word8.orb}}
         fun toWord8X i =
            case toWordAux8 i of
               Small w => ObjptrWord.toWord8X w
             | Big (isneg, _, ans) => if isneg then Word8.~ ans else ans
         fun toWord8 i = toWord8X i
         fun toInt8 i =
            case toWordAux8 i of
               Small w => ObjptrWord.toInt8X w
             | Big (isneg, extra, ans) =>
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word8.toInt8X (Word8.~ ans)
                          in
                             if Int8.>= (ans, 0)
                                then raise Overflow
                                else ans
                          end
                  else Word8.toInt8 ans
         val toWord8Unsafe = toWord8
         val toWord8XUnsafe = toWord8X
         fun toInt8Unsafe i = Word8.toInt8X (toWord8X i)

         val toWordAux16 =
            make {fromMPLimb = MPLimb.toWord16,
                  other = {wordSize = Word16.wordSize,
                           wordSizeWord = Word16.wordSizeWord,
                           zero = Word16.zero,
                           lshift = Word16.<<,
                           orb = Word16.orb}}
         fun toWord16X i =
            case toWordAux16 i of
               Small w => ObjptrWord.toWord16X w
             | Big (isneg, _, ans) => if isneg then Word16.~ ans else ans
         fun toWord16 i = toWord16X i
         fun toInt16 i =
            case toWordAux16 i of
               Small w => ObjptrWord.toInt16X w
             | Big (isneg, extra, ans) =>
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word16.toInt16X (Word16.~ ans)
                          in
                             if Int16.>= (ans, 0)
                                then raise Overflow
                                else ans
                          end
                  else Word16.toInt16 ans
         val toWord16Unsafe = toWord16
         val toWord16XUnsafe = toWord16X
         fun toInt16Unsafe i = Word16.toInt16X (toWord16X i)
                           
         val toWordAux32 =
            make {fromMPLimb = MPLimb.toWord32,
                  other = {wordSize = Word32.wordSize,
                           wordSizeWord = Word32.wordSizeWord,
                           zero = Word32.zero,
                           lshift = Word32.<<,
                           orb = Word32.orb}}
         fun toWord32X i =
            case toWordAux32 i of
               Small w => ObjptrWord.toWord32X w
             | Big (isneg, _, ans) => if isneg then Word32.~ ans else ans
         fun toWord32 i = toWord32X i
         fun toInt32 i =
            case toWordAux32 i of
               Small w => ObjptrWord.toInt32X w
             | Big (isneg, extra, ans) =>
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word32.toInt32X (Word32.~ ans)
                          in
                             if Int32.>= (ans, 0)
                                then raise Overflow
                                else ans
                          end
                  else Word32.toInt32 ans
         val toWord32Unsafe = toWord32
         val toWord32XUnsafe = toWord32X
         fun toInt32Unsafe i = Word32.toInt32X (toWord32X i)

         val toWordAux64 =
            make {fromMPLimb = MPLimb.toWord64,
                  other = {wordSize = Word64.wordSize,
                           wordSizeWord = Word64.wordSizeWord,
                           zero = Word64.zero,
                           lshift = Word64.<<,
                           orb = Word64.orb}}
         fun toWord64X i =
            case toWordAux64 i of
               Small w => ObjptrWord.toWord64X w
             | Big (isneg, _, ans) => if isneg then Word64.~ ans else ans
         fun toWord64 i = toWord64X i
         fun toInt64 i =
            case toWordAux64 i of
               Small w => ObjptrWord.toInt64X w
             | Big (isneg, extra, ans) =>
                  if extra
                     then raise Overflow
                  else if isneg
                     then let
                             val ans = Word64.toInt64X (Word64.~ ans)
                          in
                             if Int64.>= (ans, 0)
                                then raise Overflow
                                else ans
                          end
                  else Word64.toInt64 ans
         val toWord64Unsafe = toWord64
         val toWord64XUnsafe = toWord64X
         fun toInt64Unsafe i = Word64.toInt64X (toWord64X i)

         fun toIntInf i = i
         fun toIntInfUnsafe i = i
      end

      local
         val bytesPerMPLimb = Sz.fromInt32 (Int32.quot (MPLimb.wordSize, 8))
         val bytesPerCounter = Sz.fromInt32 (Int32.quot (S.precision', 8))
         val bytesPerLength = Sz.fromInt32 (Int32.quot (S.precision', 8))
         val bytesPerHeader = Sz.fromInt32 (Int32.quot (HeaderWord.wordSize, 8))
      in
         val bytesPerArrayHeader =
            Sz.+ (bytesPerCounter, Sz.+ (bytesPerLength, bytesPerHeader))
         (* Reserve heap space for a large IntInf.int with room for num + extra
          * `limbs'.  The reason for splitting this up is that extra is intended
          * to be a constant, and so can be combined at compile time.
          *)
         fun reserve (num: S.int, extra: S.int) =
            Sz.+ (Sz.* (bytesPerMPLimb, Sz.fromSeqIndex num),
            Sz.+ (Sz.* (bytesPerMPLimb, Sz.fromSeqIndex extra),
            Sz.+ (bytesPerMPLimb, (* isneg Field *)
                  bytesPerArrayHeader (* Array Header *)
            )))
      end

      (* badObjptr{Int,Word}{,Tagged} is the fixnum IntInf.int whose 
       * negation and absolute values are not fixnums. 
       * negBadIntInf is the negation (and absolute value) of that IntInf.int.
       *)
      val badObjptrInt: I.int = I.~>> (I.minInt', 0w1)
      val badObjptrWord: W.word = W.fromObjptrInt badObjptrInt
      val badObjptrWordTagged: W.word = addTag badObjptrWord
      val badObjptrIntTagged: I.int = W.toObjptrIntX badObjptrWordTagged
      val negBadIntInf: bigInt = fromObjptrInt (I.~ badObjptrInt)

      (* Given two ObjptrWord.word's, check if they have the same 'high'/'sign' bit.
       *)
      fun sameSignBit (lhs: W.word, rhs: W.word): bool =
         I.>= (W.toObjptrIntX (W.xorb (lhs, rhs)), 0)

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
                             val lhsi = W.toObjptrIntX lhsw
                             val rhsw = dropTagCoerce rhs
                             val rhsi = W.toObjptrIntX rhsw
                             val ansi = smallOp (lhsi, rhsi)
                             val answ = W.fromObjptrInt ansi
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
         val bigAdd = make (I.+, Prim.+, S.max, 1)
         val bigSub = make (I.-, Prim.-, S.max, 1)
         val bigMul = make (I.*, Prim.*, S.+, 0)
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
                    val numi = W.toObjptrIntX numw
                    val denw = dropTagCoerce den
                    val deni = W.toObjptrIntX numw
                 in
                    if numw = badObjptrWord 
                       andalso deni = ~1
                       then negBadIntInf
                       else let
                               val ansi = I.quot (numi, deni)
                               val answ = W.fromObjptrInt ansi
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
                    val numi = W.toObjptrIntX numw
                    val denw = dropTagCoerce den
                    val deni = W.toObjptrIntX numw
                    val ansi = I.rem (numi, deni)
                    val answ = W.fromObjptrInt ansi
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
         fun div2 x = I.>> (x, 0w1)
            
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
            then I.compare (W.toObjptrIntX (Prim.toWord lhs),
                            W.toObjptrIntX (Prim.toWord rhs))
            else Int32.compare (Prim.compare (lhs, rhs), 0)

      local
         fun make (smallTest, int32Test)
                  (lhs: bigInt, rhs: bigInt): bool =
            if areSmall (lhs, rhs)
               then smallTest (W.toObjptrIntX (Prim.toWord lhs),
                               W.toObjptrIntX (Prim.toWord rhs))
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
                       else if I.< (W.toObjptrIntX argw, 0)
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
         val bitsPerLimb = MPLimb.wordSizeWord
         fun shiftSize shift = S.fromWord32 (Word32.div (shift, bitsPerLimb))
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
                    val bpd = Word32.log2 (Word32.fromInt32 base)
                    val bpl = MPLimb.wordSize
                    val dpl =
                       Int32.+ (Int32.quot (bpl, bpd),
                                if Int32.mod (bpl, bpd) = 0
                                   then 0 else 1)
                 in
                    Prim.toString
                    (arg, base, 
                     Sz.+ (Sz.+ (bytesPerArrayHeader (* Array Header *),
                                 0w1 (* sign *)),
                           Sz.* (Sz.fromInt32 dpl, 
                                 Sz.fromSeqIndex (numLimbs arg))))
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

            val numLimbs = numLimbs
            val bytesPerArrayHeader = bytesPerArrayHeader
            val reserve = reserve

            val toString = Prim.toString
        end

      val abs = bigAbs
      val op +? = bigAdd
      val op + = bigAdd
      val divMod = bigDivMod
      val op div = bigDiv
      val gcd = bigGcd
      val op mod = bigMod
      val op *? = bigMul
      val op * = bigMul
      val op ~? = bigNeg
      val op ~ = bigNeg
      val quotRem = bigQuotRem
      val quot = bigQuot
      val rem = bigRem
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
      val << = bigLshift
      val notb = bigNotb
      val orb = bigOrb
      val ~>> = bigRashift
      val xorb = bigXorb

      val mkCvt = mkBigCvt
      val mkLog2 = mkBigLog2
end

structure Char8 =
   struct
      open Char8
      fun fromIntInfUnsafe i = fromInt8Unsafe (IntInf.toInt8Unsafe i)
      fun toIntInfUnsafe c = IntInf.fromInt8Unsafe (toInt8Unsafe c)
   end
structure Char16 =
   struct
      open Char16
      fun fromIntInfUnsafe i = fromInt16Unsafe (IntInf.toInt16Unsafe i)
      fun toIntInfUnsafe c = IntInf.fromInt16Unsafe (toInt16Unsafe c)
   end
structure Char32 =
   struct
      open Char32
      fun fromIntInfUnsafe i = fromInt32Unsafe (IntInf.toInt32Unsafe i)
      fun toIntInfUnsafe c = IntInf.fromInt32Unsafe (toInt32Unsafe c)
   end
structure Int8 = 
   struct
      open Int8
      val fromIntInfUnsafe = IntInf.toInt8Unsafe
      val fromIntInf = IntInf.toInt8
      val toIntInfUnsafe = IntInf.fromInt8Unsafe
      val toIntInf = IntInf.fromInt8
   end
structure Int16 = 
   struct
      open Int16
      val fromIntInfUnsafe = IntInf.toInt16Unsafe
      val fromIntInf = IntInf.toInt16
      val toIntInfUnsafe = IntInf.fromInt16Unsafe
      val toIntInf = IntInf.fromInt16
   end
structure Int32 = 
   struct
      open Int32
      val fromIntInfUnsafe = IntInf.toInt32Unsafe
      val fromIntInf = IntInf.toInt32
      val toIntInfUnsafe = IntInf.fromInt32Unsafe
      val toIntInf = IntInf.fromInt32
   end
structure Int64 = 
   struct
      open Int64
      val fromIntInfUnsafe = IntInf.toInt64Unsafe
      val fromIntInf = IntInf.toInt64
      val toIntInfUnsafe = IntInf.fromInt64Unsafe
      val toIntInf = IntInf.fromInt64
   end
structure Word8 =
   struct
      open Word8
      val fromIntInfUnsafe = IntInf.toWord8Unsafe
      val fromIntInf = IntInf.toWord8
      val toIntInfUnsafe = IntInf.fromWord8Unsafe
      val toIntInf = IntInf.fromWord8
      val toIntInfXUnsafe = IntInf.fromWord8XUnsafe
      val toIntInfX = IntInf.fromWord8X
   end
structure Word16 =
   struct
      open Word16
      val fromIntInfUnsafe = IntInf.toWord16Unsafe
      val fromIntInf = IntInf.toWord16
      val toIntInfUnsafe = IntInf.fromWord16Unsafe
      val toIntInf = IntInf.fromWord16
      val toIntInfXUnsafe = IntInf.fromWord16XUnsafe
      val toIntInfX = IntInf.fromWord16X
   end
structure Word32 =
   struct
      open Word32
      val fromIntInfUnsafe = IntInf.toWord32Unsafe
      val fromIntInf = IntInf.toWord32
      val toIntInfUnsafe = IntInf.fromWord32Unsafe
      val toIntInf = IntInf.fromWord32
      val toIntInfXUnsafe = IntInf.fromWord32XUnsafe
      val toIntInfX = IntInf.fromWord32X
   end
structure Word64 =
   struct
      open Word64
      val fromIntInfUnsafe = IntInf.toWord64Unsafe
      val fromIntInf = IntInf.toWord64
      val toIntInfUnsafe = IntInf.fromWord64Unsafe
      val toIntInf = IntInf.fromWord64
      val toIntInfXUnsafe = IntInf.fromWord64XUnsafe
      val toIntInfX = IntInf.fromWord64X
   end

end
