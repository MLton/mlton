signature MLTON_FFI =
   sig
      val atomicBegin: unit -> unit
      val atomicEnd: unit -> unit
      val getBool: int -> bool
      val getChar: int -> char
      val getInt8: int -> Int8.int
      val getInt16: int -> Int16.int
      val getInt32: int -> Int32.int
      val getInt64: int -> Int64.int
      val getReal32: int -> Real32.real
      val getReal64: int -> Real64.real
      val getWord8: int -> Word8.word
      val getWord16: int -> Word16.word
      val getWord32: int -> Word32.word
      val register: int * (unit -> unit) -> unit
      val setBool: bool -> unit
      val setChar: char -> unit
      val setInt8: Int8.int -> unit
      val setInt16: Int16.int -> unit
      val setInt32: Int32.int -> unit
      val setInt64: Int64.int -> unit
      val setReal32: Real32.real -> unit
      val setReal64: Real64.real -> unit
      val setWord8: Word8.word -> unit
      val setWord16: Word16.word -> unit
      val setWord32: Word32.word -> unit
   end
