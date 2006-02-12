(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure SeqIndex =
   struct
      open SeqIndex
         
      local
         open Primitive
         structure S =
            SeqIndex_ChooseIntN
            (type 'a t = IntInf.int -> 'a
             val fInt8 = fn i => Word8.toInt8X (IntInf.toWord8X i)
             val fInt16 = fn i => Word16.toInt16X (IntInf.toWord16X i)
             val fInt32 = fn i => Word32.toInt32X (IntInf.toWord32X i)
             val fInt64 = fn i => Word64.toInt64X (IntInf.toWord64X i))
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = fromInt8Unsafe
             val fInt16 = fromInt16Unsafe
             val fInt32 = fromInt32Unsafe
             val fInt64 = fromInt64Unsafe
             val fIntInf = S.f)
      in
         val fromIntUnsafe = S.f
      end
   
      local
         structure S =
            Int_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = toInt8Unsafe
             val fInt16 = toInt16Unsafe
             val fInt32 = toInt32Unsafe
             val fInt64 = toInt64Unsafe
             val fIntInf = toIntInf)
      in
         val toIntUnsafe = S.f
      end
   end
