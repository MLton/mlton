(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Word8:
   sig
      include WORD

      val vectorToString: t vector -> string
      val stringToVector: string -> t vector
   end =
   struct
      open Pervasive.Word8 MLton.Word8

      type t = word

      fun format (w, f) = fmt f w
      val equals = op =
      val fromChar = Byte.charToByte
      val fromIntInf = fromLargeInt
      val fromWord = fromLargeWord
      val layout = Layout.str o toString
      fun nthBitIsSet (w: t, n: int): bool =
	 0w1 = andb (0w1, >> (w, Pervasive.Word.fromInt n))
      val toChar = Byte.byteToChar
      val toIntInf = toLargeInt
      val toIntInfX = toLargeIntX
      val toWord = toLargeWord
      val toWordX = toLargeWordX

      fun vectorToString v =
	 CharVector.tabulate (Pervasive.Vector.length v, fn i =>
			      toChar (Pervasive.Vector.sub (v, i)))

      fun stringToVector s =
	 Pervasive.Vector.tabulate (Pervasive.String.size s, fn i =>
				    fromChar (Pervasive.String.sub (s, i)))
   end
