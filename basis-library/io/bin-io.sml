structure BinIO: BIN_IO_EXTRA =
   ImperativeIO (structure Array = Word8Array
		 structure ArraySlice = Word8ArraySlice
		 structure Cleaner = Cleaner
		 structure PrimIO = BinPrimIO
		 structure Vector = Word8Vector
		 structure VectorSlice = Word8VectorSlice

		 val chunkSize = Primitive.TextIO.bufSize
		 val fileTypeFlags = [PosixPrimitive.FileSys.O.binary]
		 val line = NONE
		 val mkReader = Posix.IO.mkBinReader
		 val mkWriter = Posix.IO.mkBinWriter
		 val someElem = (0wx0: Word8.word)
		 val xlatePos = SOME {fromInt = fn i => i,
				      toInt = fn i => i})
   
   
