(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Byte: BYTE =
   struct
      val byteToChar = Primitive.Word8.toChar

      val bytesToString = Primitive.String.fromWord8Vector

      val charToByte = Primitive.Char.toWord8

      fun packString (a: Word8Array.array, i: int, s: substring): unit =
	 Util.naturalForeach
	 (Substring.size s, fn j =>
	  Word8Array.update (a, i +? j, charToByte (Substring.sub (s, j))))

      val stringToBytes = Primitive.String.toWord8Vector

      local
	 fun make (length, sub) s =
	    String.tabulate (length s, fn i => byteToChar (sub (s, i)))
      in
	 val unpackString = make (Word8ArraySlice.length, Word8ArraySlice.sub)
	 val unpackStringVec =
	    make (Word8VectorSlice.length, Word8VectorSlice.sub)
      end
   end
