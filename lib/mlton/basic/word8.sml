structure Word8 : WORD =
   struct
      open Pervasive.Word8  MLton.Word8

      type t = word

      val fromChar = Byte.charToByte
      val toChar = Byte.byteToChar
      val fromWord = fromLargeWord
      val toWord = toLargeWord
      val toWordX = toLargeWordX

      val layout = Layout.str o toString

      val equals = op =

      fun nthBitIsSet (w: t, n: int): bool =
	 0w1 = andb (0w1, >> (w, Pervasive.Word.fromInt n))
   end
