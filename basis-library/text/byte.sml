(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Byte: BYTE =
   struct
      val byteToChar = Primitive.Char8.idFromWord8

      val bytesToString = Primitive.String8.idFromWord8Vector o Word8Vector.toPoly

      val charToByte = Primitive.Char8.idToWord8

      fun packString (a: Word8Array.array, i: int, s: substring): unit =
         Natural.foreach
         (Substring.size s, fn j =>
          Word8Array.update (a, i + j, charToByte (Substring.sub (s, j))))

      val stringToBytes = Word8Vector.fromPoly o Primitive.String8.idToWord8Vector

      local
         fun make (length, sub) s =
            String.tabulate (length s, fn i => byteToChar (sub (s, i)))
      in
         val unpackString = make (Word8ArraySlice.length, Word8ArraySlice.sub)
         val unpackStringVec = make (Word8VectorSlice.length, Word8VectorSlice.sub)
      end
   end
