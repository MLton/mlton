structure BinIO: BIN_IO_EXTRA =
   struct
      structure S = struct
		      structure PrimIO = BinPrimIO
		      structure Array = Word8Array
		      structure Vector = Word8Vector
		      val someElem = (0wx0: Word8.word)
		      val lineElem = (0wx0: Word8.word)
		      fun isLine _ = false
		      fun hasLine _ = false
		      structure Cleaner = Cleaner
		    end
      structure StreamIO = StreamIOExtraFile(open S)
      structure SIO = StreamIO
      structure S = struct 
		      open S 
		      structure StreamIO = StreamIO
		    end
      structure BufferI = BufferIExtraFile(open S)
      structure BI = BufferI
      structure S = struct
		      open S
		      structure BufferI = BufferI
		      val chunkSize = Primitive.TextIO.bufSize
		      val fileTypeFlags = [PosixPrimitive.FileSys.O.binary]
		      val mkReader = Posix.IO.mkBinReader
		      val mkWriter = Posix.IO.mkBinWriter
		    end
      structure ImperativeIO = ImperativeIOExtraFile(open S)
      structure FastImperativeIO = FastImperativeIOExtraFile(open S)
      open FastImperativeIO
   end
