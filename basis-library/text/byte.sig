signature BYTE =
   sig
      val byteToChar: Word8.word -> char
      val bytesToString: Word8Vector.vector -> string
      val charToByte: char -> Word8.word
      val packString: Word8Array.array * int * substring -> unit
      val stringToBytes: string -> Word8Vector.vector
      val unpackString: Word8ArraySlice.slice -> string
      val unpackStringVec: Word8VectorSlice.slice -> string
   end
