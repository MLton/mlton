(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
