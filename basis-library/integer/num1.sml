(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MKNUM1_ARG =
   sig
      type num

      val zextdFromInt8: Primitive.Int8.int -> num
      val zextdFromInt16: Primitive.Int16.int -> num
      val zextdFromInt32: Primitive.Int32.int -> num
      val zextdFromInt64: Primitive.Int64.int -> num
      val zextdFromIntInf: Primitive.IntInf.int -> num
      val zextdFromWord8: Primitive.Word8.word -> num
      val zextdFromWord16: Primitive.Word16.word -> num
      val zextdFromWord32: Primitive.Word32.word -> num
      val zextdFromWord64: Primitive.Word64.word -> num
      val zextdToInt8: num -> Primitive.Int8.int
      val zextdToInt16: num -> Primitive.Int16.int
      val zextdToInt32: num -> Primitive.Int32.int
      val zextdToInt64: num -> Primitive.Int64.int
      val zextdToIntInf: num -> Primitive.IntInf.int
      val zextdToWord8: num -> Primitive.Word8.word
      val zextdToWord16: num -> Primitive.Word16.word
      val zextdToWord32: num -> Primitive.Word32.word
      val zextdToWord64: num -> Primitive.Word64.word

      val sextdFromInt8: Primitive.Int8.int -> num
      val sextdFromInt16: Primitive.Int16.int -> num
      val sextdFromInt32: Primitive.Int32.int -> num
      val sextdFromInt64: Primitive.Int64.int -> num
      val sextdFromIntInf: Primitive.IntInf.int -> num
      val sextdFromWord8: Primitive.Word8.word -> num
      val sextdFromWord16: Primitive.Word16.word -> num
      val sextdFromWord32: Primitive.Word32.word -> num
      val sextdFromWord64: Primitive.Word64.word -> num
      val sextdToInt8: num -> Primitive.Int8.int
      val sextdToInt16: num -> Primitive.Int16.int
      val sextdToInt32: num -> Primitive.Int32.int
      val sextdToInt64: num -> Primitive.Int64.int
      val sextdToIntInf: num -> Primitive.IntInf.int
      val sextdToWord8: num -> Primitive.Word8.word
      val sextdToWord16: num -> Primitive.Word16.word
      val sextdToWord32: num -> Primitive.Word32.word
      val sextdToWord64: num -> Primitive.Word64.word

      val castFromInt8: Primitive.Int8.int -> num
      val castFromInt16: Primitive.Int16.int -> num
      val castFromInt32: Primitive.Int32.int -> num
      val castFromInt64: Primitive.Int64.int -> num
      val castFromIntInf: Primitive.IntInf.int -> num
      val castFromWord8: Primitive.Word8.word -> num
      val castFromWord16: Primitive.Word16.word -> num
      val castFromWord32: Primitive.Word32.word -> num
      val castFromWord64: Primitive.Word64.word -> num
      val castToInt8: num -> Primitive.Int8.int
      val castToInt16: num -> Primitive.Int16.int
      val castToInt32: num -> Primitive.Int32.int
      val castToInt64: num -> Primitive.Int64.int
      val castToIntInf: num -> Primitive.IntInf.int
      val castToWord8: num -> Primitive.Word8.word
      val castToWord16: num -> Primitive.Word16.word
      val castToWord32: num -> Primitive.Word32.word
      val castToWord64: num -> Primitive.Word64.word

      val zchckFromInt8: Primitive.Int8.int -> num
      val zchckFromInt16: Primitive.Int16.int -> num
      val zchckFromInt32: Primitive.Int32.int -> num
      val zchckFromInt64: Primitive.Int64.int -> num
      val zchckFromIntInf: Primitive.IntInf.int -> num
      val zchckFromWord8: Primitive.Word8.word -> num
      val zchckFromWord16: Primitive.Word16.word -> num
      val zchckFromWord32: Primitive.Word32.word -> num
      val zchckFromWord64: Primitive.Word64.word -> num
      val zchckToInt8: num -> Primitive.Int8.int
      val zchckToInt16: num -> Primitive.Int16.int
      val zchckToInt32: num -> Primitive.Int32.int
      val zchckToInt64: num -> Primitive.Int64.int
      val zchckToIntInf: num -> Primitive.IntInf.int
      val zchckToWord8: num -> Primitive.Word8.word
      val zchckToWord16: num -> Primitive.Word16.word
      val zchckToWord32: num -> Primitive.Word32.word
      val zchckToWord64: num -> Primitive.Word64.word

      val schckFromInt8: Primitive.Int8.int -> num
      val schckFromInt16: Primitive.Int16.int -> num
      val schckFromInt32: Primitive.Int32.int -> num
      val schckFromInt64: Primitive.Int64.int -> num
      val schckFromIntInf: Primitive.IntInf.int -> num
      val schckFromWord8: Primitive.Word8.word -> num
      val schckFromWord16: Primitive.Word16.word -> num
      val schckFromWord32: Primitive.Word32.word -> num
      val schckFromWord64: Primitive.Word64.word -> num
      val schckToInt8: num -> Primitive.Int8.int
      val schckToInt16: num -> Primitive.Int16.int
      val schckToInt32: num -> Primitive.Int32.int
      val schckToInt64: num -> Primitive.Int64.int
      val schckToIntInf: num -> Primitive.IntInf.int
      val schckToWord8: num -> Primitive.Word8.word
      val schckToWord16: num -> Primitive.Word16.word
      val schckToWord32: num -> Primitive.Word32.word
      val schckToWord64: num -> Primitive.Word64.word
   end
signature MKNUM1_RES =
   sig
      type num

      val zextdFromInt: Int.int -> num
      val zextdToInt: num -> Int.int
      val sextdFromInt: Int.int -> num
      val sextdToInt: num -> Int.int
      val castFromInt: Int.int -> num
      val castToInt: num -> Int.int
      val zchckFromInt: Int.int -> num
      val zchckToInt: num -> Int.int
      val schckFromInt: Int.int -> num
      val schckToInt: num -> Int.int

      val zextdFromFixedInt: FixedInt.int -> num
      val zextdToFixedInt: num -> FixedInt.int
      val sextdFromFixedInt: FixedInt.int -> num
      val sextdToFixedInt: num -> FixedInt.int
      val castFromFixedInt: FixedInt.int -> num
      val castToFixedInt: num -> FixedInt.int
      val zchckFromFixedInt: FixedInt.int -> num
      val zchckToFixedInt: num -> FixedInt.int
      val schckFromFixedInt: FixedInt.int -> num
      val schckToFixedInt: num -> FixedInt.int

      val zextdFromLargeInt: LargeInt.int -> num
      val zextdToLargeInt: num -> LargeInt.int
      val sextdFromLargeInt: LargeInt.int -> num
      val sextdToLargeInt: num -> LargeInt.int
      val castFromLargeInt: LargeInt.int -> num
      val castToLargeInt: num -> LargeInt.int
      val zchckFromLargeInt: LargeInt.int -> num
      val zchckToLargeInt: num -> LargeInt.int
      val schckFromLargeInt: LargeInt.int -> num
      val schckToLargeInt: num -> LargeInt.int

      val zextdFromWord: Word.word -> num
      val zextdToWord: num -> Word.word
      val sextdFromWord: Word.word -> num
      val sextdToWord: num -> Word.word
      val castFromWord: Word.word -> num
      val castToWord: num -> Word.word
      val zchckFromWord: Word.word -> num
      val zchckToWord: num -> Word.word
      val schckFromWord: Word.word -> num
      val schckToWord: num -> Word.word

      val zextdFromLargeWord: LargeWord.word -> num
      val zextdToLargeWord: num -> LargeWord.word
      val sextdFromLargeWord: LargeWord.word -> num
      val sextdToLargeWord: num -> LargeWord.word
      val castFromLargeWord: LargeWord.word -> num
      val castToLargeWord: num -> LargeWord.word
      val zchckFromLargeWord: LargeWord.word -> num
      val zchckToLargeWord: num -> LargeWord.word
      val schckFromLargeWord: LargeWord.word -> num
      val schckToLargeWord: num -> LargeWord.word

      val zextdFromSysWord: SysWord.word -> num
      val zextdToSysWord: num -> SysWord.word
      val sextdFromSysWord: SysWord.word -> num
      val sextdToSysWord: num -> SysWord.word
      val castFromSysWord: SysWord.word -> num
      val castToSysWord: num -> SysWord.word
      val zchckFromSysWord: SysWord.word -> num
      val zchckToSysWord: num -> SysWord.word
      val schckFromSysWord: SysWord.word -> num
      val schckToSysWord: num -> SysWord.word
   end
signature PRIM_INTEGER =
   sig
      include PRIM_INTEGER
      include MKNUM1_RES where type num = int
   end
signature PRIM_WORD =
   sig
      include PRIM_WORD
      include MKNUM1_RES where type num = word
   end

functor MkNum1 (I: MKNUM1_ARG) : MKNUM1_RES =
   struct
      open I

      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.zextdFromInt8
             val fInt16 = I.zextdFromInt16
             val fInt32 = I.zextdFromInt32
             val fInt64 = I.zextdFromInt64
             val fIntInf = I.zextdFromIntInf)
      in
         val zextdFromInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.zextdToInt8
             val fInt16 = I.zextdToInt16
             val fInt32 = I.zextdToInt32
             val fInt64 = I.zextdToInt64
             val fIntInf = I.zextdToIntInf)
      in
         val zextdToInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.sextdFromInt8
             val fInt16 = I.sextdFromInt16
             val fInt32 = I.sextdFromInt32
             val fInt64 = I.sextdFromInt64
             val fIntInf = I.sextdFromIntInf)
      in
         val sextdFromInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.sextdToInt8
             val fInt16 = I.sextdToInt16
             val fInt32 = I.sextdToInt32
             val fInt64 = I.sextdToInt64
             val fIntInf = I.sextdToIntInf)
      in
         val sextdToInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.castFromInt8
             val fInt16 = I.castFromInt16
             val fInt32 = I.castFromInt32
             val fInt64 = I.castFromInt64
             val fIntInf = I.castFromIntInf)
      in
         val castFromInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.castToInt8
             val fInt16 = I.castToInt16
             val fInt32 = I.castToInt32
             val fInt64 = I.castToInt64
             val fIntInf = I.castToIntInf)
      in
         val castToInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.zchckFromInt8
             val fInt16 = I.zchckFromInt16
             val fInt32 = I.zchckFromInt32
             val fInt64 = I.zchckFromInt64
             val fIntInf = I.zchckFromIntInf)
      in
         val zchckFromInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.zchckToInt8
             val fInt16 = I.zchckToInt16
             val fInt32 = I.zchckToInt32
             val fInt64 = I.zchckToInt64
             val fIntInf = I.zchckToIntInf)
      in
         val zchckToInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.schckFromInt8
             val fInt16 = I.schckFromInt16
             val fInt32 = I.schckFromInt32
             val fInt64 = I.schckFromInt64
             val fIntInf = I.schckFromIntInf)
      in
         val schckFromInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.schckToInt8
             val fInt16 = I.schckToInt16
             val fInt32 = I.schckToInt32
             val fInt64 = I.schckToInt64
             val fIntInf = I.schckToIntInf)
      in
         val schckToInt = S.f
      end


      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = 'a -> num
             val fInt8 = I.zextdFromInt8
             val fInt16 = I.zextdFromInt16
             val fInt32 = I.zextdFromInt32
             val fInt64 = I.zextdFromInt64)
      in
         val zextdFromFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = num -> 'a
             val fInt8 = I.zextdToInt8
             val fInt16 = I.zextdToInt16
             val fInt32 = I.zextdToInt32
             val fInt64 = I.zextdToInt64)
      in
         val zextdToFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = 'a -> num
             val fInt8 = I.sextdFromInt8
             val fInt16 = I.sextdFromInt16
             val fInt32 = I.sextdFromInt32
             val fInt64 = I.sextdFromInt64)
      in
         val sextdFromFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = num -> 'a
             val fInt8 = I.sextdToInt8
             val fInt16 = I.sextdToInt16
             val fInt32 = I.sextdToInt32
             val fInt64 = I.sextdToInt64)
      in
         val sextdToFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = 'a -> num
             val fInt8 = I.castFromInt8
             val fInt16 = I.castFromInt16
             val fInt32 = I.castFromInt32
             val fInt64 = I.castFromInt64)
      in
         val castFromFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = num -> 'a
             val fInt8 = I.castToInt8
             val fInt16 = I.castToInt16
             val fInt32 = I.castToInt32
             val fInt64 = I.castToInt64)
      in
         val castToFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = 'a -> num
             val fInt8 = I.zchckFromInt8
             val fInt16 = I.zchckFromInt16
             val fInt32 = I.zchckFromInt32
             val fInt64 = I.zchckFromInt64)
      in
         val zchckFromFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = num -> 'a
             val fInt8 = I.zchckToInt8
             val fInt16 = I.zchckToInt16
             val fInt32 = I.zchckToInt32
             val fInt64 = I.zchckToInt64)
      in
         val zchckToFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = 'a -> num
             val fInt8 = I.schckFromInt8
             val fInt16 = I.schckFromInt16
             val fInt32 = I.schckFromInt32
             val fInt64 = I.schckFromInt64)
      in
         val schckFromFixedInt = S.f
      end
      local
         structure S =
            FixedInt_ChooseIntN
            (type 'a t = num -> 'a
             val fInt8 = I.schckToInt8
             val fInt16 = I.schckToInt16
             val fInt32 = I.schckToInt32
             val fInt64 = I.schckToInt64)
      in
         val schckToFixedInt = S.f
      end


      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.zextdFromInt8
             val fInt16 = I.zextdFromInt16
             val fInt32 = I.zextdFromInt32
             val fInt64 = I.zextdFromInt64
             val fIntInf = I.zextdFromIntInf)
      in
         val zextdFromLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.zextdToInt8
             val fInt16 = I.zextdToInt16
             val fInt32 = I.zextdToInt32
             val fInt64 = I.zextdToInt64
             val fIntInf = I.zextdToIntInf)
      in
         val zextdToLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.sextdFromInt8
             val fInt16 = I.sextdFromInt16
             val fInt32 = I.sextdFromInt32
             val fInt64 = I.sextdFromInt64
             val fIntInf = I.sextdFromIntInf)
      in
         val sextdFromLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.sextdToInt8
             val fInt16 = I.sextdToInt16
             val fInt32 = I.sextdToInt32
             val fInt64 = I.sextdToInt64
             val fIntInf = I.sextdToIntInf)
      in
         val sextdToLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.castFromInt8
             val fInt16 = I.castFromInt16
             val fInt32 = I.castFromInt32
             val fInt64 = I.castFromInt64
             val fIntInf = I.castFromIntInf)
      in
         val castFromLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.castToInt8
             val fInt16 = I.castToInt16
             val fInt32 = I.castToInt32
             val fInt64 = I.castToInt64
             val fIntInf = I.castToIntInf)
      in
         val castToLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.zchckFromInt8
             val fInt16 = I.zchckFromInt16
             val fInt32 = I.zchckFromInt32
             val fInt64 = I.zchckFromInt64
             val fIntInf = I.zchckFromIntInf)
      in
         val zchckFromLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.zchckToInt8
             val fInt16 = I.zchckToInt16
             val fInt32 = I.zchckToInt32
             val fInt64 = I.zchckToInt64
             val fIntInf = I.zchckToIntInf)
      in
         val zchckToLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> num
             val fInt8 = I.schckFromInt8
             val fInt16 = I.schckFromInt16
             val fInt32 = I.schckFromInt32
             val fInt64 = I.schckFromInt64
             val fIntInf = I.schckFromIntInf)
      in
         val schckFromLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = num -> 'a
             val fInt8 = I.schckToInt8
             val fInt16 = I.schckToInt16
             val fInt32 = I.schckToInt32
             val fInt64 = I.schckToInt64
             val fIntInf = I.schckToIntInf)
      in
         val schckToLargeInt = S.f
      end


      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.zextdFromWord8
             val fWord16 = I.zextdFromWord16
             val fWord32 = I.zextdFromWord32
             val fWord64 = I.zextdFromWord64)
      in
         val zextdFromWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.zextdToWord8
             val fWord16 = I.zextdToWord16
             val fWord32 = I.zextdToWord32
             val fWord64 = I.zextdToWord64)
      in
         val zextdToWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.sextdFromWord8
             val fWord16 = I.sextdFromWord16
             val fWord32 = I.sextdFromWord32
             val fWord64 = I.sextdFromWord64)
      in
         val sextdFromWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.sextdToWord8
             val fWord16 = I.sextdToWord16
             val fWord32 = I.sextdToWord32
             val fWord64 = I.sextdToWord64)
      in
         val sextdToWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.castFromWord8
             val fWord16 = I.castFromWord16
             val fWord32 = I.castFromWord32
             val fWord64 = I.castFromWord64)
      in
         val castFromWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.castToWord8
             val fWord16 = I.castToWord16
             val fWord32 = I.castToWord32
             val fWord64 = I.castToWord64)
      in
         val castToWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.zchckFromWord8
             val fWord16 = I.zchckFromWord16
             val fWord32 = I.zchckFromWord32
             val fWord64 = I.zchckFromWord64)
      in
         val zchckFromWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.zchckToWord8
             val fWord16 = I.zchckToWord16
             val fWord32 = I.zchckToWord32
             val fWord64 = I.zchckToWord64)
      in
         val zchckToWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.schckFromWord8
             val fWord16 = I.schckFromWord16
             val fWord32 = I.schckFromWord32
             val fWord64 = I.schckFromWord64)
      in
         val schckFromWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.schckToWord8
             val fWord16 = I.schckToWord16
             val fWord32 = I.schckToWord32
             val fWord64 = I.schckToWord64)
      in
         val schckToWord = S.f
      end


      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.zextdFromWord8
             val fWord16 = I.zextdFromWord16
             val fWord32 = I.zextdFromWord32
             val fWord64 = I.zextdFromWord64)
      in
         val zextdFromLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.zextdToWord8
             val fWord16 = I.zextdToWord16
             val fWord32 = I.zextdToWord32
             val fWord64 = I.zextdToWord64)
      in
         val zextdToLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.sextdFromWord8
             val fWord16 = I.sextdFromWord16
             val fWord32 = I.sextdFromWord32
             val fWord64 = I.sextdFromWord64)
      in
         val sextdFromLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.sextdToWord8
             val fWord16 = I.sextdToWord16
             val fWord32 = I.sextdToWord32
             val fWord64 = I.sextdToWord64)
      in
         val sextdToLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.castFromWord8
             val fWord16 = I.castFromWord16
             val fWord32 = I.castFromWord32
             val fWord64 = I.castFromWord64)
      in
         val castFromLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.castToWord8
             val fWord16 = I.castToWord16
             val fWord32 = I.castToWord32
             val fWord64 = I.castToWord64)
      in
         val castToLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.zchckFromWord8
             val fWord16 = I.zchckFromWord16
             val fWord32 = I.zchckFromWord32
             val fWord64 = I.zchckFromWord64)
      in
         val zchckFromLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.zchckToWord8
             val fWord16 = I.zchckToWord16
             val fWord32 = I.zchckToWord32
             val fWord64 = I.zchckToWord64)
      in
         val zchckToLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.schckFromWord8
             val fWord16 = I.schckFromWord16
             val fWord32 = I.schckFromWord32
             val fWord64 = I.schckFromWord64)
      in
         val schckFromLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.schckToWord8
             val fWord16 = I.schckToWord16
             val fWord32 = I.schckToWord32
             val fWord64 = I.schckToWord64)
      in
         val schckToLargeWord = S.f
      end


      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.zextdFromWord8
             val fWord16 = I.zextdFromWord16
             val fWord32 = I.zextdFromWord32
             val fWord64 = I.zextdFromWord64)
      in
         val zextdFromSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.zextdToWord8
             val fWord16 = I.zextdToWord16
             val fWord32 = I.zextdToWord32
             val fWord64 = I.zextdToWord64)
      in
         val zextdToSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.sextdFromWord8
             val fWord16 = I.sextdFromWord16
             val fWord32 = I.sextdFromWord32
             val fWord64 = I.sextdFromWord64)
      in
         val sextdFromSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.sextdToWord8
             val fWord16 = I.sextdToWord16
             val fWord32 = I.sextdToWord32
             val fWord64 = I.sextdToWord64)
      in
         val sextdToSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.castFromWord8
             val fWord16 = I.castFromWord16
             val fWord32 = I.castFromWord32
             val fWord64 = I.castFromWord64)
      in
         val castFromSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.castToWord8
             val fWord16 = I.castToWord16
             val fWord32 = I.castToWord32
             val fWord64 = I.castToWord64)
      in
         val castToSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.zchckFromWord8
             val fWord16 = I.zchckFromWord16
             val fWord32 = I.zchckFromWord32
             val fWord64 = I.zchckFromWord64)
      in
         val zchckFromSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.zchckToWord8
             val fWord16 = I.zchckToWord16
             val fWord32 = I.zchckToWord32
             val fWord64 = I.zchckToWord64)
      in
         val zchckToSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> num
             val fWord8 = I.schckFromWord8
             val fWord16 = I.schckFromWord16
             val fWord32 = I.schckFromWord32
             val fWord64 = I.schckFromWord64)
      in
         val schckFromSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = num -> 'a
             val fWord8 = I.schckToWord8
             val fWord16 = I.schckToWord16
             val fWord32 = I.schckToWord32
             val fWord64 = I.schckToWord64)
      in
         val schckToSysWord = S.f
      end
   end

structure Primitive =
struct
open Primitive

structure Int8 = 
   struct
      open Int8
      local
         structure S = MkNum1(struct 
                                 open Int8 
                                 type num = int 
                              end)
      in
         open S
      end
   end
structure Int16 = 
   struct
      open Int16
      local
         structure S = MkNum1(struct 
                                 open Int16
                                 type num = int 
                              end)
      in
         open S
      end
   end
structure Int32 = 
   struct
      open Int32
      local
         structure S = MkNum1(struct 
                                 open Int32
                                 type num = int 
                              end)
      in
         open S
      end
   end
structure Int64 = 
   struct
      open Int64
      local
         structure S = MkNum1(struct 
                                 open Int64
                                 type num = int 
                              end)
      in
         open S
      end
   end
structure IntInf = 
   struct
      open IntInf
      local
         structure S = MkNum1(struct 
                                 open IntInf
                                 type num = int 
                              end)
      in
         open S
      end
   end

structure Word8 = 
   struct
      open Word8
      local
         structure S = MkNum1(struct 
                                 open Word8 
                                 type num = word 
                              end)
      in
         open S
      end
   end
structure Word16 = 
   struct
      open Word16
      local
         structure S = MkNum1(struct 
                                 open Word16
                                 type num = word 
                              end)
      in
         open S
      end
   end
structure Word32 = 
   struct
      open Word32
      local
         structure S = MkNum1(struct 
                                 open Word32
                                 type num = word 
                              end)
      in
         open S
      end
   end
structure Word64 : PRIM_WORD = 
   struct
      open Word64
      local
         structure S = MkNum1(struct 
                                 open Word64
                                 type num = word 
                              end)
      in
         open S
      end
   end

end
