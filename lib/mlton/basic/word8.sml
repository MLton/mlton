structure Word8 =
   struct
      open Pervasive.Word8

      type t = word

      val fromChar = Byte.charToByte
      val toChar = Byte.byteToChar
      val fromWord = fromLargeWord
      val toWord = toLargeWord

      val layout = Layout.str o toString
   end
