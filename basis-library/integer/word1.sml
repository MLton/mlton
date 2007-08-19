(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WORD_FROM_TO_ARG =
   sig
      type word
      (* Lowbits or sign extend. *)
      val fromInt8: Primitive.Int8.int -> word
      val fromInt16: Primitive.Int16.int -> word
      val fromInt32: Primitive.Int32.int -> word
      val fromInt64: Primitive.Int64.int -> word
      val fromIntInf: Primitive.IntInf.int -> word
      (* Lowbits or zero extend. *)
      val fromInt8Z: Primitive.Int8.int -> word
      val fromInt16Z: Primitive.Int16.int -> word
      val fromInt32Z: Primitive.Int32.int -> word
      val fromInt64Z: Primitive.Int64.int -> word
      val fromIntInfZ: Primitive.IntInf.int -> word
      (* Lowbits or zero extend. *)
      val fromWord8: Primitive.Word8.word -> word
      val fromWord16: Primitive.Word16.word -> word
      val fromWord32: Primitive.Word32.word -> word
      val fromWord64: Primitive.Word64.word -> word
      (* Lowbits or sign extend. *)
      val fromWord8X: Primitive.Word8.word -> word
      val fromWord16X: Primitive.Word16.word -> word
      val fromWord32X: Primitive.Word32.word -> word
      val fromWord64X: Primitive.Word64.word -> word
      (* Overflow checking, unsigned interp. *)
      val toInt8: word -> Primitive.Int8.int
      val toInt16: word -> Primitive.Int16.int
      val toInt32: word -> Primitive.Int32.int
      val toInt64: word -> Primitive.Int64.int
      val toIntInf: word -> Primitive.IntInf.int
      (* Overflow checking, signed interp. *)
      val toInt8X: word -> Primitive.Int8.int
      val toInt16X: word -> Primitive.Int16.int
      val toInt32X: word -> Primitive.Int32.int
      val toInt64X: word -> Primitive.Int64.int   
      val toIntInfX: word -> Primitive.IntInf.int   
      (* Lowbits or zero extend. *)
      val toWord8: word -> Primitive.Word8.word
      val toWord16: word -> Primitive.Word16.word
      val toWord32: word -> Primitive.Word32.word
      val toWord64: word -> Primitive.Word64.word
      (* Lowbits or sign extend. *)
      val toWord8X: word -> Primitive.Word8.word
      val toWord16X: word -> Primitive.Word16.word
      val toWord32X: word -> Primitive.Word32.word
      val toWord64X: word -> Primitive.Word64.word
   end

signature WORD_FROM_TO_RES =   
   sig
      type word

      val fromInt: Int.int -> word
      val fromIntZ: Int.int -> word
      val fromLargeInt: LargeInt.int -> word
      val fromLargeIntZ: LargeInt.int -> word
      val fromWord: Word.word -> word
      val fromWordX: Word.word -> word
      val fromLargeWord: LargeWord.word -> word
      val fromLarge: LargeWord.word -> word
      val fromLargeWordX: LargeWord.word -> word
      val fromLargeX: LargeWord.word -> word
      val fromSysWord: SysWord.word -> word
      val fromSysWordX: SysWord.word -> word

      val toInt: word -> Int.int
      val toIntX: word -> Int.int
      val toLargeInt: word -> LargeInt.int
      val toLargeIntX: word -> LargeInt.int
      val toWord: word -> Word.word
      val toWordX: word -> Word.word
      val toLargeWord: word -> LargeWord.word
      val toLarge: word -> LargeWord.word
      val toLargeWordX: word -> LargeWord.word
      val toLargeX: word -> LargeWord.word
      val toSysWord: word -> SysWord.word
      val toSysWordX: word -> SysWord.word
   end

functor WordFromTo (W: WORD_FROM_TO_ARG): WORD_FROM_TO_RES where type word = W.word =
   struct
      open W

      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> word
             val fInt8 = W.fromInt8
             val fInt16 = W.fromInt16
             val fInt32 = W.fromInt32
             val fInt64 = W.fromInt64
             val fIntInf = W.fromIntInf)
      in
         val fromInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> word
             val fInt8 = W.fromInt8Z
             val fInt16 = W.fromInt16Z
             val fInt32 = W.fromInt32Z
             val fInt64 = W.fromInt64Z
             val fIntInf = W.fromIntInfZ)
      in
         val fromIntZ = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> word
             val fInt8 = W.fromInt8
             val fInt16 = W.fromInt16
             val fInt32 = W.fromInt32
             val fInt64 = W.fromInt64
             val fIntInf = W.fromIntInf)
      in
         val fromLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> word
             val fInt8 = W.fromInt8Z
             val fInt16 = W.fromInt16Z
             val fInt32 = W.fromInt32Z
             val fInt64 = W.fromInt64Z
             val fIntInf = W.fromIntInfZ)
      in
         val fromLargeIntZ = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> word
             val fWord8 = W.fromWord8
             val fWord16 = W.fromWord16
             val fWord32 = W.fromWord32
             val fWord64 = W.fromWord64)
      in
         val fromWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> word
             val fWord8 = W.fromWord8X
             val fWord16 = W.fromWord16X
             val fWord32 = W.fromWord32X
             val fWord64 = W.fromWord64X)
      in
         val fromWordX = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> word
             val fWord8 = W.fromWord8
             val fWord16 = W.fromWord16
             val fWord32 = W.fromWord32
             val fWord64 = W.fromWord64)
      in
         val fromLargeWord = S.f
         val fromLarge = fromLargeWord
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> word
             val fWord8 = W.fromWord8X
             val fWord16 = W.fromWord16X
             val fWord32 = W.fromWord32X
             val fWord64 = W.fromWord64X)
      in
         val fromLargeWordX = S.f
         val fromLargeX = fromLargeWordX
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> word
             val fWord8 = W.fromWord8
             val fWord16 = W.fromWord16
             val fWord32 = W.fromWord32
             val fWord64 = W.fromWord64)
      in
         val fromSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> word
             val fWord8 = W.fromWord8X
             val fWord16 = W.fromWord16X
             val fWord32 = W.fromWord32X
             val fWord64 = W.fromWord64X)
      in
         val fromSysWordX = S.f
      end

      local
         structure S =
            Int_ChooseInt
            (type 'a t = word -> 'a
             val fInt8 = W.toInt8
             val fInt16 = W.toInt16
             val fInt32 = W.toInt32
             val fInt64 = W.toInt64
             val fIntInf = W.toIntInf)
      in
         val toInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = word -> 'a
             val fInt8 = W.toInt8X
             val fInt16 = W.toInt16X
             val fInt32 = W.toInt32X
             val fInt64 = W.toInt64X
             val fIntInf = W.toIntInfX)
      in
         val toIntX = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = word -> 'a
             val fInt8 = W.toInt8
             val fInt16 = W.toInt16
             val fInt32 = W.toInt32
             val fInt64 = W.toInt64
             val fIntInf = W.toIntInf)
      in
         val toLargeInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = word -> 'a
             val fInt8 = W.toInt8X
             val fInt16 = W.toInt16X
             val fInt32 = W.toInt32X
             val fInt64 = W.toInt64X
             val fIntInf = W.toIntInfX)
      in
         val toLargeIntX = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = word -> 'a
             val fWord8 = W.toWord8
             val fWord16 = W.toWord16
             val fWord32 = W.toWord32
             val fWord64 = W.toWord64)
      in
         val toWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = word -> 'a
             val fWord8 = W.toWord8X
             val fWord16 = W.toWord16X
             val fWord32 = W.toWord32X
             val fWord64 = W.toWord64X)
      in
         val toWordX = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = word -> 'a
             val fWord8 = W.toWord8
             val fWord16 = W.toWord16
             val fWord32 = W.toWord32
             val fWord64 = W.toWord64)
      in
         val toLargeWord = S.f
         val toLarge = toLargeWord
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = word -> 'a
             val fWord8 = W.toWord8X
             val fWord16 = W.toWord16X
             val fWord32 = W.toWord32X
             val fWord64 = W.toWord64X)
      in
         val toLargeWordX = S.f
         val toLargeX = toLargeWordX
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = word -> 'a
             val fWord8 = W.toWord8
             val fWord16 = W.toWord16
             val fWord32 = W.toWord32
             val fWord64 = W.toWord64)
      in
         val toSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = word -> 'a
             val fWord8 = W.toWord8X
             val fWord16 = W.toWord16X
             val fWord32 = W.toWord32X
             val fWord64 = W.toWord64X)
      in
         val toSysWordX = S.f
      end
   end

structure Primitive = struct
open Primitive

structure Word8 = struct
                     open Word8
                     local
                        structure S = WordFromTo (Primitive.Word8)
                     in
                        open S
                     end
                  end
structure Word16 = struct
                      open Word16
                      local
                         structure S = WordFromTo (Primitive.Word16)
                      in
                         open S
                      end
                   end
structure Word32 = struct
                      open Word32
                      local
                         structure S = WordFromTo (Primitive.Word32)
                      in
                         open S
                      end
                   end
structure Word64 = struct
                      open Word64
                      local
                         structure S = WordFromTo (Primitive.Word64)
                      in
                         open S
                      end
                   end
end
