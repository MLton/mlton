(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Byte: BYTE =
   struct
      open Primitive.Byte Int
	 
      val bytesToString = Primitive.String.fromWord8Vector
      val stringToBytes = Primitive.String.toWord8Vector
      val unpackStringVec = bytesToString o Word8Vector.extract
      val unpackString = bytesToString o Word8Array.extract
      fun packString (a: Word8Array.array, i: int, s: substring): unit =
	 Util.naturalForeach
	 (Substring.size s, fn j =>
	  Word8Array.update (a, i +? j, charToByte (Substring.sub (s, j))))
   end
