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
      val areSmall: int * int -> bool

      val maxInt: int option
      val minInt: int option

      val zero: int
      val one: int

      val abs: int -> int
      val + : int * int -> int
      val divMod: int * int -> int * int
      val div: int * int -> int
      val gcd: int * int -> int
      val mod: int * int -> int
      val * : int * int -> int
      val ~ : int -> int
      val quotRem: int * int -> int * int
      val quot: int * int -> int
      val rem: int * int -> int
      val - : int * int -> int

      val < : int * int -> bool
      val <= : int * int -> bool
      val > : int * int -> bool
      val >= : int * int -> bool
      val compare: int * int -> Primitive.Order.order
      val min: int * int -> int
      val max: int * int -> int

      val andb: int * int -> int
      val << : int * Primitive.Word32.word -> int
      val notb: int -> int
      val orb: int * int -> int
      val ~>> : int * Primitive.Word32.word -> int
      val xorb: int * int -> int

      val toString8: int -> Primitive.String8.string

      val fromInt8: Primitive.Int8.int -> int
      val fromInt16: Primitive.Int16.int -> int
      val fromInt32: Primitive.Int32.int -> int
      val fromInt64: Primitive.Int64.int -> int
      val fromIntInf: Primitive.IntInf.int -> int

      val fromWord8: Primitive.Word8.word -> int
      val fromWord16: Primitive.Word16.word -> int
      val fromWord32: Primitive.Word32.word -> int
      val fromWord64: Primitive.Word64.word -> int

      val fromWordX8: Primitive.Word8.word -> int
      val fromWordX16: Primitive.Word16.word -> int
      val fromWordX32: Primitive.Word32.word -> int
      val fromWordX64: Primitive.Word64.word -> int

      val toInt8: int -> Primitive.Int8.int
      val toInt16: int -> Primitive.Int16.int
      val toInt32: int -> Primitive.Int32.int
      val toInt64: int -> Primitive.Int64.int
      val toIntInf: int -> Primitive.IntInf.int

      val toWord8: int -> Primitive.Word8.word
      val toWord16: int -> Primitive.Word16.word
      val toWord32: int -> Primitive.Word32.word
      val toWord64: int -> Primitive.Word64.word

      val toWordX8: int -> Primitive.Word8.word
      val toWordX16: int -> Primitive.Word16.word
      val toWordX32: int -> Primitive.Word32.word
      val toWordX64: int -> Primitive.Word64.word
   end

structure Primitive = struct

open Primitive

structure IntInf : INT_INF0 =
   struct
      structure Prim = Primitive.IntInf

      structure A = Primitive.Array
      structure V = Primitive.Vector
      structure S = SeqIndex

      structure W = ObjptrWord
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
      fun dropTagCoerceInt (i: bigInt): I.int = W.toIntXEq (dropTagCoerce i)
      fun addTag (w: W.word): W.word = W.orb (W.<< (w, 0w1), 0w1)
      fun addTagCoerce (w: W.word): bigInt = Prim.fromWord (addTag w)
      fun addTagCoerceInt (i: I.int): bigInt = addTagCoerce (W.fromIntEq i)
      fun zeroTag (w: W.word): W.word = W.andb (w, W.notb 0w1)
      fun oneTag (w: W.word): W.word = W.orb (w, 0w1)
      fun oneTagCoerce (w: W.word): bigInt = Prim.fromWord (oneTag w)

      fun rep i =
         if isSmall i
            then Small (dropTagCoerceInt i)
            else Big (Prim.toVector i)

      fun 'a buildBigInt {toMPLimb: 'a -> MPLimb.word,
                          other : {zero: 'a,
                                   eq: 'a * 'a -> bool,
                                   rshift: 'a * Word32.word -> 'a}} 
                         (isneg, ans) =
         let
            fun loop (ans, i, acc) =
               if (#eq other) (ans, (#zero other))
                  then (i, acc)
                  else let
                          val limb = toMPLimb ans
                          val ans = (#rshift other) (ans, MPLimb.wordSizeWord')
                       in
                          loop (ans, S.+ (i, 1), (i, limb) :: acc)
                       end
            val (n, acc) = loop (ans, 1, [(0, if isneg then 0w1 else 0w0)])
            val a = A.array n
            fun loop acc =
               case acc of
                  [] => ()
                | (i, v) :: acc => (A.update (a, i, v)
                                    ; loop acc)
            val () = loop acc
         in
            Prim.fromVector (V.fromArray a)
         end

      local
         fun 'a make {toMPLimb: 'a -> MPLimb.word,
                      toObjptrWord: 'a -> ObjptrWord.word,
                      toObjptrWordX: 'a -> ObjptrWord.word,
                      other : {precision': Int32.int,
                               zero: 'a,
                               one: 'a,
                               neg: 'a -> 'a,
                               eq: 'a * 'a -> bool,
                               lt: 'a * 'a -> bool,
                               rashift: 'a * Word32.word -> 'a,
                               rshift: 'a * Word32.word -> 'a}} =
            let
               fun fromInt i =
                  if Int32.> (ObjptrWord.wordSize', #precision' other)
                     then Prim.fromWord (addTag (toObjptrWordX i))
                     else let
                             val upperBits =
                                (#rashift other)
                                (i, Word32.- (ObjptrWord.wordSizeWord', 0w2))
                          in
                             if (#eq other) (upperBits, #zero other)
                                orelse (#eq other) (upperBits, (#neg other) (#one other))
                                then Prim.fromWord (addTag (toObjptrWord i))
                                else let
                                        val (isneg, ans) = 
                                           if (#lt other) (i, (#zero other))
                                              then (true, (#neg other) i)
                                              else (false, i)
                                     in
                                        buildBigInt 
                                        {toMPLimb = toMPLimb,
                                         other = {zero = #zero other,
                                                  eq = #eq other,
                                                  rshift = #rshift other}}
                                        (isneg, ans)
                                     end
                          end
            in
               fromInt
            end
      in
         val fromInt8 =
            make {toMPLimb = MPLimb.fromIntZ8,
                  toObjptrWord = ObjptrWord.fromIntZ8,
                  toObjptrWordX = ObjptrWord.fromInt8,
                  other = {precision' = Int8.precision',
                           zero = Int8.zero,
                           one = Int8.one,
                           neg = Int8.~,
                           eq = ((op =) : Int8.int * Int8.int -> bool),
                           lt = Int8.<,
                           rashift = Int8.~>>,
                           rshift = Int8.>>}}
         val fromInt16 =
            make {toMPLimb = MPLimb.fromIntZ16,
                  toObjptrWord = ObjptrWord.fromIntZ16,
                  toObjptrWordX = ObjptrWord.fromInt16,
                  other = {precision' = Int16.precision',
                           zero = Int16.zero,
                           one = Int16.one,
                           neg = Int16.~,
                           eq = ((op =) : Int16.int * Int16.int -> bool),
                           lt = Int16.<,
                           rashift = Int16.~>>,
                           rshift = Int16.>>}}
         val fromInt32 =
            make {toMPLimb = MPLimb.fromIntZ32,
                  toObjptrWord = ObjptrWord.fromIntZ32,
                  toObjptrWordX = ObjptrWord.fromInt32,
                  other = {precision' = Int32.precision',
                           zero = Int32.zero,
                           one = Int32.one,
                           neg = Int32.~,
                           eq = ((op =) : Int32.int * Int32.int -> bool),
                           lt = Int32.<,
                           rashift = Int32.~>>,
                           rshift = Int32.>>}}
         val fromInt64 =
            make {toMPLimb = MPLimb.fromIntZ64,
                  toObjptrWord = ObjptrWord.fromIntZ64,
                  toObjptrWordX = ObjptrWord.fromInt64,
                  other = {precision' = Int64.precision',
                           zero = Int64.zero,
                           one = Int64.one,
                           neg = Int64.~,
                           eq = ((op =) : Int64.int * Int64.int -> bool),
                           lt = Int64.<,
                           rashift = Int64.~>>,
                           rshift = Int64.>>}}
         val fromIntInf = fn i => i
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
         fun 'a make {toMPLimb: 'a -> MPLimb.word,
                      toObjptrWord: 'a -> ObjptrWord.word,
                      other : {wordSize': Int32.int,
                               zero: 'a,
                               one: 'a,
                               eq: 'a * 'a -> bool,
                               lt: 'a * 'a -> bool,
                               rshift: 'a * Word32.word -> 'a}} =
            let
               fun fromWord w =
                  if Int32.> (ObjptrWord.wordSize', #wordSize' other)
                     then Prim.fromWord (addTag (toObjptrWord w))
                     else let
                             val upperBits =
                                (#rshift other)
                                (w, Word32.- (ObjptrWord.wordSizeWord', 0w2))
                          in
                             if (#eq other) (upperBits, #zero other)
                                then Prim.fromWord (addTag (toObjptrWord w))
                                else let
                                        val ans = w
                                     in
                                        buildBigInt 
                                        {toMPLimb = toMPLimb,
                                         other = {zero = #zero other,
                                                  eq = #eq other,
                                                  rshift = #rshift other}}
                                        (false, ans)
                                     end
                          end
            in
               fromWord
            end
      in
         val fromWord8 =
            make {toMPLimb = MPLimb.fromWord8,
                  toObjptrWord = ObjptrWord.fromWord8,
                  other = {wordSize' = Word8.wordSize',
                           zero = Word8.zero,
                           one = Word8.one,
                           eq = ((op =) : Word8.word * Word8.word -> bool),
                           lt = Word8.<,
                           rshift = Word8.>>}}
         val fromWord16 =
            make {toMPLimb = MPLimb.fromWord16,
                  toObjptrWord = ObjptrWord.fromWord16,
                  other = {wordSize' = Word16.wordSize',
                           zero = Word16.zero,
                           one = Word16.one,
                           eq = ((op =) : Word16.word * Word16.word -> bool),
                           lt = Word16.<,
                           rshift = Word16.>>}}
         val fromWord32 =
            make {toMPLimb = MPLimb.fromWord32,
                  toObjptrWord = ObjptrWord.fromWord32,
                  other = {wordSize' = Word32.wordSize',
                           zero = Word32.zero,
                           one = Word32.one,
                           eq = ((op =) : Word32.word * Word32.word -> bool),
                           lt = Word32.<,
                           rshift = Word32.>>}}
         val fromWord64 =
            make {toMPLimb = MPLimb.fromWord64,
                  toObjptrWord = ObjptrWord.fromWord64,
                  other = {wordSize' = Word64.wordSize',
                           zero = Word64.zero,
                           one = Word64.one,
                           eq = ((op =) : Word64.word * Word64.word -> bool),
                           lt = Word64.<,
                           rshift = Word64.>>}}
      end

      val fromWordX8 : Word8.word -> bigInt =
         fn w => fromInt8 (Int8.fromWordX8 w)
      val fromWordX16 : Word16.word -> bigInt =
         fn w => fromInt16 (Int16.fromWordX16 w)
      val fromWordX32 : Word32.word -> bigInt =
         fn w => fromInt32 (Int32.fromWordX32 w)
      val fromWordX64 : Word64.word -> bigInt =
         fn w => fromInt64 (Int64.fromWordX64 w)

      local
         fun 'a make {fromMPLimb: MPLimb.word -> 'a,
                      fromObjptrWordX: ObjptrWord.word -> 'a, 
                      other : {precision': Int32.int,
                               zero: 'a,
                               lshift: 'a * Word32.word -> 'a,
                               neg: 'a -> 'a,
                               orb: 'a * 'a -> 'a}} =
            let
               val limbsPer =
                  if Int32.>= (MPLimb.wordSize', #precision' other)
                     then 1
                     else S.fromInt32 (Int32.quot (#precision' other, MPLimb.wordSize'))
               fun toInt i =
                  if isSmall i
                     then fromObjptrWordX (dropTagCoerce i)
                     else if Int32.> (ObjptrWord.wordSize', #precision' other)
                             then raise Overflow
                             else 
                             let
                                val v = Prim.toVector i
                                val n = V.length v
                                val isneg = V.sub (v, 0) <> 0w0
                                val ans =
                                   if S.> (S.- (n, 1), limbsPer)
                                      then raise Overflow
                                   else if Int32.>= (MPLimb.wordSize', #precision' other)
                                      then fromMPLimb (V.sub (v, 1))
                                   else 
                                   let
                                      fun loop (i, ans) =
                                         if S.> (i, 0)
                                            then let
                                                    val ans = 
                                                       (#orb other) 
                                                       ((#lshift other) 
                                                        (ans, MPLimb.wordSizeWord'),
                                                        fromMPLimb (V.sub (v, i)))
                                                 in
                                                    loop (S.- (i, 1), ans)
                                                 end
                                            else ans
                                   in
                                      loop (S.- (n, 1), #zero other)
                                   end
                             in
                                if isneg then (#neg other) ans else ans
                             end
            in
               toInt
            end
      in
         val toInt8 =
            make {fromMPLimb = MPLimb.toInt8,
                  fromObjptrWordX = ObjptrWord.toIntX8,
                  other = {precision' = Int8.precision',
                           zero = Int8.zero,
                           lshift = Int8.<<,
                           neg = Int8.~,
                           orb = Int8.orb}}
         val toInt16 =
            make {fromMPLimb = MPLimb.toInt16,
                  fromObjptrWordX = ObjptrWord.toIntX16,
                  other = {precision' = Int16.precision',
                           zero = Int16.zero,
                           lshift = Int16.<<,
                           neg = Int16.~,
                           orb = Int16.orb}}
         val toInt32 =
            make {fromMPLimb = MPLimb.toInt32,
                  fromObjptrWordX = ObjptrWord.toIntX32,
                  other = {precision' = Int32.precision',
                           zero = Int32.zero,
                           lshift = Int32.<<,
                           neg = Int32.~,
                           orb = Int32.orb}}
         val toInt64 =
            make {fromMPLimb = MPLimb.toInt64,
                  fromObjptrWordX = ObjptrWord.toIntX64,
                  other = {precision' = Int64.precision',
                           zero = Int64.zero,
                           lshift = Int64.<<,
                           neg = Int64.~,
                           orb = Int64.orb}}
         val toIntInf = fn i => i
      end

      local
         fun 'a make {fromMPLimb: MPLimb.word -> 'a,
                      fromObjptrWordX: ObjptrWord.word -> 'a, 
                      other : {wordSize': Int32.int,
                               zero: 'a,
                               lshift: 'a * Word32.word -> 'a,
                               neg: 'a -> 'a,
                               orb: 'a * 'a -> 'a}} = 
            let
               val limbsPer =
                  if Int32.>= (MPLimb.wordSize', #wordSize' other)
                     then 1
                     else S.fromInt32 (Int32.quot (#wordSize' other, MPLimb.wordSize'))
               fun toWord i =
                  if isSmall i
                     then fromObjptrWordX (dropTagCoerce i)
                     else let
                             val v = Prim.toVector i
                             val n = V.length v
                             val isneg = V.sub (v, 0) <> 0w0
                             val ans =
                                let
                                   fun loop (i, ans) =
                                      if S.> (i, 0)
                                         then let
                                                 val ans = 
                                                    (#orb other) 
                                                    ((#lshift other) 
                                                     (ans, MPLimb.wordSizeWord'),
                                                     fromMPLimb (V.sub (v, i)))
                                              in
                                                 loop (S.- (i, 1), ans)
                                              end
                                         else ans
                                in
                                   loop (S.min (S.- (n, 1), limbsPer), #zero other)
                                end
                          in
                             if isneg then (#neg other) ans else ans
                          end
            in
               toWord
            end
      in
         val toWord8 =
            make {fromMPLimb = MPLimb.toWord8,
                  fromObjptrWordX = ObjptrWord.toWordX8,
                  other = {wordSize' = Word8.wordSize',
                           zero = Word8.zero,
                           lshift = Word8.<<,
                           neg = Word8.~,
                           orb = Word8.orb}}
         val toWordX8 = toWord8
         val toWord16 =
            make {fromMPLimb = MPLimb.toWord16,
                  fromObjptrWordX = ObjptrWord.toWordX16,
                  other = {wordSize' = Word16.wordSize',
                           zero = Word16.zero,
                           lshift = Word16.<<,
                           neg = Word16.~,
                           orb = Word16.orb}}
         val toWordX16 = toWord16
         val toWord32 =
            make {fromMPLimb = MPLimb.toWord32,
                  fromObjptrWordX = ObjptrWord.toWordX32,
                  other = {wordSize' = Word32.wordSize',
                           zero = Word32.zero,
                           lshift = Word32.<<,
                           neg = Word32.~,
                           orb = Word32.orb}}
         val toWordX32 = toWord32
         val toWord64 =
            make {fromMPLimb = MPLimb.toWord64,
                  fromObjptrWordX = ObjptrWord.toWordX64,
                  other = {wordSize' = Word64.wordSize',
                           zero = Word64.zero,
                           lshift = Word64.<<,
                           neg = Word64.~,
                           orb = Word64.orb}}
         val toWordX64 = toWord64
      end

      local
         val bytesPerMPLimb = Sz.fromInt32 (Int32.quot (MPLimb.wordSize', 8))
         val bytesPerCounter = Sz.fromInt32 (Int32.quot (S.precision', 8))
         val bytesPerLength = Sz.fromInt32 (Int32.quot (S.precision', 8))
         val bytesPerHeader = Sz.fromInt32 4
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
      val badObjptrWord: W.word = W.fromIntEq badObjptrInt
      val badObjptrWordTagged: W.word = addTag badObjptrWord
      val badObjptrIntTagged: I.int = W.toIntXEq badObjptrWordTagged
      val negBadIntInf: bigInt = fromObjptrInt (I.~ badObjptrInt)

      (* Given two ObjptrWord.word's, check if they have the same `high'/'sign' bit.
       *)
      fun sameSignBit (lhs: W.word, rhs: W.word): bool =
         I.>= (W.toIntXEq (W.xorb (lhs, rhs)), 0)

      (* Given a bignum bigint, test if it is (strictly) negative.
       *)
      fun bigIsNeg (arg: bigInt): bool =
         V.sub (Prim.toVector arg, 0) <> 0w0

      local
         fun make (smallOp, bigOp, limbsFn, extra)
                  (lhs: bigInt, rhs: bigInt): bigInt =
            let
               val res =
                  if areSmall (lhs, rhs)
                     then let
                             val lhsw = dropTagCoerce lhs
                             val lhsi = W.toIntXEq lhsw
                             val rhsw = dropTagCoerce rhs
                             val rhsi = W.toIntXEq rhsw
                             val ansi = smallOp (lhsi, rhsi)
                             val answ = W.fromIntEq ansi
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
                    val numi = W.toIntXEq numw
                    val denw = dropTagCoerce den
                    val deni = W.toIntXEq numw
                 in
                    if numw = badObjptrWord 
                       andalso deni = ~1
                       then negBadIntInf
                       else let
                               val ansi = I.quot (numi, deni)
                               val answ = W.fromIntEq ansi
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
                                               reserve (S.- (nlimbs, dlimbs), 1))
                 end

      fun bigRem (num: bigInt, den: bigInt): bigInt =
         if areSmall (num, den)
            then let 
                    val numw = dropTagCoerce num
                    val numi = W.toIntXEq numw
                    val denw = dropTagCoerce den
                    val deni = W.toIntXEq numw
                    val ansi = I.rem (numi, deni)
                    val answ = W.fromIntEq ansi
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
            
         fun gcdInt (a, b, acc) =
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
                                    then gcdInt (a_2, b_2, acc + acc)
                                    else gcdInt (a_2, b, acc)
                              else
                                 if 0 = b_r2
                                    then gcdInt (a, b_2, acc)
                                    else
                                       if a >= b
                                          then gcdInt (div2 (a - b), b, acc)
                                          else gcdInt (a, div2 (b - a), acc)
                        end
      in
         fun bigGcd (lhs: bigInt, rhs: bigInt): bigInt =
            if areSmall (lhs, rhs)
               then addTagCoerceInt (gcdInt (I.abs (dropTagCoerceInt lhs),
                                             I.abs (dropTagCoerceInt rhs),
                                             1))
               else Prim.gcd (lhs, rhs, 
                              reserve (S.max (numLimbs lhs, numLimbs rhs), 0))
      end


      fun bigCompare (lhs: bigInt, rhs: bigInt): order =
         if areSmall (lhs, rhs)
            then I.compare (W.toIntXEq (Prim.toWord lhs),
                            W.toIntXEq (Prim.toWord rhs))
            else Int32.compare (Prim.compare (lhs, rhs), 0)

      local
         fun make (smallTest, int32Test)
                  (lhs: bigInt, rhs: bigInt): bool =
            if areSmall (lhs, rhs)
               then smallTest (W.toIntXEq (Prim.toWord lhs),
                               W.toIntXEq (Prim.toWord rhs))
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
                       else if I.< (W.toIntXEq argw, 0)
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

      fun bigSign' (arg: bigInt): Int32.int =
         if isSmall arg
            then I.sign' (dropTagCoerceInt arg)
            else if bigIsNeg arg
                    then ~1
                    else 1

      fun bigSameSign (lhs: bigInt, rhs: bigInt): bool =
         bigSign' lhs = bigSign' rhs

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
         val bitsPerLimb = MPLimb.wordSizeWord'
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

      fun bigToString8 (arg: bigInt): String8.string =
         Prim.toString
         (arg, 10, Sz.+ (bytesPerArrayHeader (* Array Header *),
                         Sz.+ (0w2, (* sign *)
                               Sz.* (0w10, Sz.fromSeqIndex (numLimbs arg)))))

      type int = bigInt
      type t = int

      val maxInt = NONE
      val minInt = NONE

      val abs = bigAbs
      val op + = bigAdd
      val divMod = bigDivMod
      val op div = bigDiv
      val gcd = bigGcd
      val op mod = bigMod
      val op * = bigMul
      val op ~ = bigNeg
      val quotRem = bigQuotRem
      val quot = bigQuot
      val rem = bigRem
      val op - = bigSub

      val op < = bigLT
      val op <= = bigLE
      val op > = bigGT
      val op >= = bigGE
      val compare = bigCompare
      val min = bigMin
      val max = bigMax

      val andb = bigAndb
      val << = bigLshift
      val notb = bigNotb
      val orb = bigOrb
      val ~>> = bigRashift
      val xorb = bigXorb

      val toString8 = bigToString8
end

structure Int8 = 
   struct
      open Int8
      val fromIntInf = IntInf.toInt8
      val toIntInf = IntInf.fromInt8
   end
structure Int16 = 
   struct
      open Int16
      val fromIntInf = IntInf.toInt16
      val toIntInf = IntInf.fromInt16
   end
structure Int32 = 
   struct
      open Int32
      val fromIntInf = IntInf.toInt32
      val toIntInf = IntInf.fromInt32
   end
structure Int64 = 
   struct
      open Int64
      val fromIntInf = IntInf.toInt64
      val toIntInf = IntInf.fromInt64
   end
structure Word8 =
   struct
      open Word8
      val fromIntInf = IntInf.toWord8
      val toIntInf = IntInf.fromWord8
      val toIntInfX = IntInf.fromWordX8
   end
structure Word16 =
   struct
      open Word16
      val fromIntInf = IntInf.toWord16
      val toIntInf = IntInf.fromWord16
      val toIntInfX = IntInf.fromWordX16
   end
structure Word32 =
   struct
      open Word32
      val fromIntInf = IntInf.toWord32
      val toIntInf = IntInf.fromWord32
      val toIntInfX = IntInf.fromWordX32
   end
structure Word64 =
   struct
      open Word64
      val fromIntInf = IntInf.toWord64
      val toIntInf = IntInf.fromWord64
      val toIntInfX = IntInf.fromWordX64
   end

end

(*
(*
 * IntInf.int's either have a bottom bit of 1, in which case the top 31
 * bits are the signed integer, or else the bottom bit is 0, in which case
 * they point to an vector of Word.word's.  The first word is either 0,
 * indicating that the number is positive, or 1, indicating that it is
 * negative.  The rest of the vector contains the `limbs' (big digits) of
 * the absolute value of the number, from least to most significant.
 *)
structure IntInf: INT_INF_EXTRA =
   struct

      (*
       * bigInt toString and fmt.
       * dpc is the maximum number of digits per `limb'.
       *)
      local
         open StringCvt

         fun cvt {base: smallInt,
                  dpc: word,
                  smallCvt: smallInt -> string}
            (arg: bigInt)
            : string =
            if isSmall arg
               then smallCvt (Word.toIntX (stripTag arg))
            else Prim.toString (arg, base,
                                Word.+
                                (reserve (0, 0),
                                 Word.+ (0w2, (* sign character *)
                                         Word.* (dpc,
                                                 Word.fromInt (bigSize arg)))))
         val binCvt = cvt {base = 2, dpc = 0w32, smallCvt = Int.fmt BIN}
         val octCvt = cvt {base = 8, dpc = 0w11, smallCvt = Int.fmt OCT}
         val hexCvt = cvt {base = 16, dpc = 0w8, smallCvt = Int.fmt HEX}
      in
         val bigToString = cvt {base = 10,
                                dpc = 0w10,
                                smallCvt = Int.toString}
         fun bigFmt radix =
            case radix of
               BIN => binCvt
             | OCT => octCvt
             | DEC => bigToString
             | HEX => hexCvt
      end

      (*
       * bigInt scan and fromString.
       *)
      local
         open StringCvt

         (*
          * We use Word.word to store chunks of digits.
          * smallToInf converts such a word to a fixnum bigInt.
          * Thus, it can only represent values in [- 2^30, 2^30).
          *)
         fun smallToBig (arg: Word.word): bigInt =
            Prim.fromWord (addTag arg)
            
            
         (*
          * Given a char, if it is a digit in the appropriate base,
          * convert it to a word.  Otherwise, return NONE.
          * Note, both a-f and A-F are accepted as hexadecimal digits.
          *)
         fun binDig (ch: char): Word.word option =
            case ch of
               #"0" => SOME 0w0
             | #"1" => SOME 0w1
             | _ => NONE

         local
            val op <= = Char.<=
         in
            fun octDig (ch: char): Word.word option =
               if #"0" <= ch andalso ch <= #"7"
                  then SOME (Word.fromInt (ord ch -? ord #"0"))
               else NONE
                  
            fun decDig (ch: char): Word.word option =
               if #"0" <= ch andalso ch <= #"9"
                  then SOME (Word.fromInt (ord ch -? ord #"0"))
               else NONE
                  
            fun hexDig (ch: char): Word.word option =
               if #"0" <= ch andalso ch <= #"9"
                  then SOME (Word.fromInt (ord ch -? ord #"0"))
               else if #"a" <= ch andalso ch <= #"f"
                       then SOME (Word.fromInt (ord ch -? (ord #"a" - 0xa)))
                    else if #"A" <= ch andalso ch <= #"F"
                            then SOME (Word.fromInt
                                       (ord ch -? (ord #"A" - 0xA)))
                         else
                            NONE
         end
      
         (*
          * Given a digit converter and a char reader, return a digit
          * reader.
          *)
         fun toDigR (charToDig: char -> Word.word option,
                     cread: (char, 'a) reader)
            (s: 'a)
            : (Word.word * 'a) option =
            case cread s of
               NONE => NONE
             | SOME (ch, s') =>
                  case charToDig ch of
                     NONE => NONE
                   | SOME dig => SOME (dig, s')
                        
         (*
          * A chunk represents the result of processing some digits.
          * more is a bool indicating if there might be more digits.
          * shift is base raised to the number-of-digits-seen power.
          * chunk is the value of the digits seen.
          *)
         type chunk = {
                       more: bool,
                       shift: Word.word,
                       chunk: Word.word
                       }
            
         (*
          * Given the base, the number of digits per chunk,
          * a char reader and a digit reader, return a chunk reader.
          *)
         fun toChunkR (base: Word.word,
                       dpc: smallInt,
                       dread: (Word.word, 'a) reader)
            : (chunk, 'a) reader =
            let fun loop {left: smallInt,
                          shift: Word.word,
                          chunk: Word.word,
                          s: 'a}
               : chunk * 'a =
               if left <= 0
                  then ({more = true,
                         shift = shift,
                         chunk = chunk },
                        s)
               else
                  case dread s of
                     NONE => ({more = false,
                               shift = shift,
                               chunk = chunk},
                              s)
                   | SOME (dig, s') =>
                        loop {
                              left = left - 1,
                              shift = Word.* (base, shift),
                              chunk = Word.+ (Word.* (base,
                                                      chunk),
                                              dig),
                              s = s'
                              }
                fun reader (s: 'a): (chunk * 'a) option =
                   case dread s of
                      NONE => NONE
                    | SOME (dig, next) =>
                         SOME (loop {left = dpc - 1,
                                     shift = base,
                                     chunk = dig,
                                     s = next})
            in reader
            end
         
         (*
          * Given a chunk reader, return an unsigned reader.
          *)
         fun toUnsR (ckread: (chunk, 'a) reader): (bigInt, 'a) reader =
            let fun loop (more: bool, ac: bigInt, s: 'a) =
               if more
                  then case ckread s of
                     NONE => (ac, s)
                   | SOME ({more, shift, chunk}, s') =>
                        loop (more,
                              bigPlus (bigMul (smallToBig shift,
                                               ac),
                                       smallToBig chunk),
                              s')
               else (ac, s)
                fun reader (s: 'a): (bigInt * 'a) option =
                   case ckread s of
                      NONE => NONE
                    | SOME ({more, chunk, ...}, s') =>
                         SOME (loop (more,
                                     smallToBig chunk,
                                     s'))
            in reader
            end
         
         (*
          * Given a char reader and an unsigned reader, return an unsigned
          * reader that includes skipping the option hex '0x'.
          *)
         fun toHexR (cread: (char, 'a) reader, uread: (bigInt, 'a) reader) 
            s =
            case cread s of
               NONE => NONE
             | SOME (c1, s1) =>
                  if c1 = #"0" then
                     case cread s1 of
                        NONE => SOME (zero, s1)
                      | SOME (c2, s2) =>
                           if c2 = #"x" orelse c2 = #"X" then
                              case uread s2 of 
                                 NONE => SOME (zero, s1)
                               | SOME x => SOME x
                           else uread s
                  else uread s

         (*
          * Given a char reader and an unsigned reader, return a signed
          * reader.  This includes skipping any initial white space.
          *)
         fun toSign (cread: (char, 'a) reader, uread: (bigInt, 'a) reader)
            : (bigInt, 'a) reader =
            let
               fun reader (s: 'a): (bigInt * 'a) option =
                  case cread s of
                     NONE => NONE
                   | SOME (ch, s') =>
                        if Char.isSpace ch then reader s'
                        else
                           let
                              val (isNeg, s'') =
                                 case ch of
                                    #"+" => (false, s')
                                  | #"-" => (true, s')
                                  | #"~" => (true, s')
                                  | _ => (false, s)
                           in
                              if isNeg then
                                 case uread s'' of
                                    NONE => NONE
                                  | SOME (abs, s''') =>
                                       SOME (bigNegate abs, s''')
                              else uread s''
                           end
            in
               reader
            end
                  
         (*
          * Base-specific conversions from char readers to
          * bigInt readers.
          *)
         local
            fun reader (base, dpc, dig)
               (cread: (char, 'a) reader): (bigInt, 'a) reader =
               let val dread = toDigR (dig, cread)
                  val ckread = toChunkR (base, dpc, dread)
                  val uread = toUnsR ckread
                  val hread =
                     if base = 0w16 then toHexR (cread, uread) else uread
                  val reader = toSign (cread, hread)
               in reader
               end
         in
            fun binReader z = reader (0w2, 29, binDig) z
            fun octReader z = reader (0w8, 9, octDig) z
            fun decReader z = reader (0w10, 9, decDig) z
            fun hexReader z = reader (0w16, 7, hexDig) z
         end     
      in
         
         local fun stringReader (pos, str) =
            if pos >= String.size str
               then NONE
            else SOME (String.sub (str, pos), (pos + 1, str))
               val reader = decReader stringReader
         in
            fun bigFromString str =
               case reader (0, str) of
                  NONE => NONE
                | SOME (res, _) => SOME res
         end
      
         fun bigScan radix =
            case radix of
               BIN => binReader
             | OCT => octReader
             | DEC => decReader
             | HEX => hexReader
      end

      local
         fun isEven (n: int) = Int.mod (Int.abs n, 2) = 0
      in
         fun pow (i: bigInt, j: int): bigInt =
            if j < 0 then
               if i = zero then
                  raise Div
               else
                  if i = one then one
                  else if i = negOne then if isEven j then one else negOne
                  else zero
            else
               if j = 0 then one
               else
                  let
                     fun square (n: bigInt): bigInt = bigMul (n, n)
                     (* pow (j) returns (i ^ j) *)
                     fun pow (j: int): bigInt =
                        if j <= 0 then one
                        else if isEven j then evenPow j
                        else bigMul (i, evenPow (j - 1))
                     (* evenPow (j) returns (i ^ j), assuming j is even *)
                     and evenPow (j: int): bigInt =
                        square (pow (Int.quot (j, 2)))
                  in pow (j)
                  end
      end


      (*
       * bigInt log2
       *)
      structure Word =
         struct
            open Word
            fun log2 (w: word): int =
               let
                  fun loop (n, s, ac): word =
                     if n = 0w1
                        then ac
                     else
                        let
                           val (n, ac) =
                              if n >= << (0w1, s)
                                 then (>> (n, s), ac + s)
                              else (n, ac)
                        in
                           loop (n, >> (s, 0w1), ac)
                        end
               in
                  toInt (loop (w, 0w16, 0w0))
               end
         end

      local
         val bitsPerLimb: Int.int = 32
      in
         fun log2 (n: bigInt): Int.int =
            if bigLE (n, 0)
               then raise Domain
            else
               case rep n of
                  Big v =>
                     Int.+ (Int.* (bitsPerLimb, Int.- (Vector.length v, 2)),
                            Word.log2 (Vector.sub (v, Int.- (Vector.length v, 1))))
                | Small i => Word.log2 (Word.fromInt i)
      end

   
   end

structure LargeInt = IntInf
*)
