(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INT_FROM_TO_ARG =
   sig
      type int
      (* Lowbits or sign-extend. *)
      val fromInt8Unsafe: Primitive.Int8.int -> int
      val fromInt16Unsafe: Primitive.Int16.int -> int
      val fromInt32Unsafe: Primitive.Int32.int -> int
      val fromInt64Unsafe: Primitive.Int64.int -> int
      val fromIntInfUnsafe: Primitive.IntInf.int -> int
      (* Overflow checking, signed interp. *)
      val fromInt8: Primitive.Int8.int -> int
      val fromInt16: Primitive.Int16.int -> int
      val fromInt32: Primitive.Int32.int -> int
      val fromInt64: Primitive.Int64.int -> int
      val fromIntInf: Primitive.IntInf.int -> int
      (* Overflow checking, unsigned interp. *)
      val fromWord8: Primitive.Word8.word -> int
      val fromWord16: Primitive.Word16.word -> int
      val fromWord32: Primitive.Word32.word -> int
      val fromWord64: Primitive.Word64.word -> int
      (* Overflow checking, signed interp. *)
      val fromWord8X: Primitive.Word8.word -> int
      val fromWord16X: Primitive.Word16.word -> int
      val fromWord32X: Primitive.Word32.word -> int
      val fromWord64X: Primitive.Word64.word -> int
      (* Lowbits or sign-extend. *)
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
      (* Lowbits or zero extend. *)
      val toWord8: int -> Primitive.Word8.word
      val toWord16: int -> Primitive.Word16.word
      val toWord32: int -> Primitive.Word32.word
      val toWord64: int -> Primitive.Word64.word
      (* Lowbits or sign extend. *)
      val toWord8X: int -> Primitive.Word8.word
      val toWord16X: int -> Primitive.Word16.word
      val toWord32X: int -> Primitive.Word32.word
      val toWord64X: int -> Primitive.Word64.word
   end

signature INT_FROM_TO_RES =
   sig
      type int

      val fromIntUnsafe: Int.int -> int
      val fromInt: Int.int -> int
      val fromLargeInt: LargeInt.int -> int
      val fromLarge: LargeInt.int -> int
      val fromWord: Word.word -> int
      val fromWordX: Word.word -> int
      val fromLargeWord: LargeWord.word -> int
      val fromLargeWordX: LargeWord.word -> int
      val fromSysWord: SysWord.word -> int
      val fromSysWordX: SysWord.word -> int

      val toIntUnsafe: int -> Int.int
      val toInt: int -> Int.int
      val toLargeInt: int -> LargeInt.int
      val toLarge: int -> LargeInt.int
      val toWord: int -> Word.word
      val toWordX: int -> Word.word
      val toLargeWord: int -> LargeWord.word
      val toLargeWordX: int -> LargeWord.word
      val toSysWord: int -> SysWord.word
      val toSysWordX: int -> SysWord.word
   end

functor IntFromTo(I: INT_FROM_TO_ARG): INT_FROM_TO_RES where type int = I.int =
   struct
      open I

      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = I.fromInt8Unsafe
             val fInt16 = I.fromInt16Unsafe
             val fInt32 = I.fromInt32Unsafe
             val fInt64 = I.fromInt64Unsafe
             val fIntInf = I.fromIntInfUnsafe)
      in
         val fromIntUnsafe = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = I.fromInt8
             val fInt16 = I.fromInt16
             val fInt32 = I.fromInt32
             val fInt64 = I.fromInt64
             val fIntInf = I.fromIntInf)
      in
         val fromInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = I.fromInt8
             val fInt16 = I.fromInt16
             val fInt32 = I.fromInt32
             val fInt64 = I.fromInt64
             val fIntInf = I.fromIntInf)
      in
         val fromLargeInt = S.f
         val fromLarge = fromLargeInt
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> int
             val fWord8 = I.fromWord8
             val fWord16 = I.fromWord16
             val fWord32 = I.fromWord32
             val fWord64 = I.fromWord64)
      in
         val fromWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = 'a -> int
             val fWord8 = I.fromWord8X
             val fWord16 = I.fromWord16X
             val fWord32 = I.fromWord32X
             val fWord64 = I.fromWord64X)
      in
         val fromWordX = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> int
             val fWord8 = I.fromWord8
             val fWord16 = I.fromWord16
             val fWord32 = I.fromWord32
             val fWord64 = I.fromWord64)
      in
         val fromLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = 'a -> int
             val fWord8 = I.fromWord8X
             val fWord16 = I.fromWord16X
             val fWord32 = I.fromWord32X
             val fWord64 = I.fromWord64X)
      in
         val fromLargeWordX = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> int
             val fWord8 = I.fromWord8
             val fWord16 = I.fromWord16
             val fWord32 = I.fromWord32
             val fWord64 = I.fromWord64)
      in
         val fromSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = 'a -> int
             val fWord8 = I.fromWord8X
             val fWord16 = I.fromWord16X
             val fWord32 = I.fromWord32X
             val fWord64 = I.fromWord64X)
      in
         val fromSysWordX = S.f
      end

      local
         structure S =
            Int_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = I.toInt8Unsafe
             val fInt16 = I.toInt16Unsafe
             val fInt32 = I.toInt32Unsafe
             val fInt64 = I.toInt64Unsafe
             val fIntInf = I.toIntInfUnsafe)
      in
         val toIntUnsafe = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = I.toInt8
             val fInt16 = I.toInt16
             val fInt32 = I.toInt32
             val fInt64 = I.toInt64
             val fIntInf = I.toIntInf)
      in
         val toInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = I.toInt8
             val fInt16 = I.toInt16
             val fInt32 = I.toInt32
             val fInt64 = I.toInt64
             val fIntInf = I.toIntInf)
      in
         val toLargeInt = S.f
         val toLarge = toLargeInt
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = int -> 'a
             val fWord8 = I.toWord8
             val fWord16 = I.toWord16
             val fWord32 = I.toWord32
             val fWord64 = I.toWord64)
      in
         val toWord = S.f
      end
      local
         structure S =
            Word_ChooseWordN
            (type 'a t = int -> 'a
             val fWord8 = I.toWord8X
             val fWord16 = I.toWord16X
             val fWord32 = I.toWord32X
             val fWord64 = I.toWord64X)
      in
         val toWordX = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = int -> 'a
             val fWord8 = I.toWord8
             val fWord16 = I.toWord16
             val fWord32 = I.toWord32
             val fWord64 = I.toWord64)
      in
         val toLargeWord = S.f
      end
      local
         structure S =
            LargeWord_ChooseWordN
            (type 'a t = int -> 'a
             val fWord8 = I.toWord8X
             val fWord16 = I.toWord16X
             val fWord32 = I.toWord32X
             val fWord64 = I.toWord64X)
      in
         val toLargeWordX = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = int -> 'a
             val fWord8 = I.toWord8
             val fWord16 = I.toWord16
             val fWord32 = I.toWord32
             val fWord64 = I.toWord64)
      in
         val toSysWord = S.f
      end
      local
         structure S =
            SysWord_ChooseWordN
            (type 'a t = int -> 'a
             val fWord8 = I.toWord8X
             val fWord16 = I.toWord16X
             val fWord32 = I.toWord32X
             val fWord64 = I.toWord64X)
      in
         val toSysWordX = S.f
      end
   end

structure Primitive = struct
open Primitive

structure Int8 = struct 
                    open Int8
                    local 
                       structure S = IntFromTo (Primitive.Int8)
                    in
                       open S
                    end
                 end
structure Int16 = struct 
                     open Int16
                     local 
                        structure S = IntFromTo (Primitive.Int16)
                     in
                        open S
                     end
                  end
structure Int32 = struct 
                     open Int32
                     local 
                        structure S = IntFromTo (Primitive.Int32)
                     in
                        open S
                     end
                  end
structure Int64 = struct 
                     open Int64
                     local 
                        structure S = IntFromTo (Primitive.Int64)
                     in
                        open S
                     end
                  end
structure IntInf = struct 
                      open IntInf
                      local 
                         structure S = IntFromTo (Primitive.IntInf)
                      in
                         open S
                      end
                   end
end
