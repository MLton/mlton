structure TextIO: TEXT_IO_EXTRA =
   struct
      structure IO =
	 ImperativeIO (structure Array = CharArray
		       structure ArraySlice = CharArraySlice
		       structure Cleaner = Cleaner
		       structure PrimIO = TextPrimIO
		       structure Vector = CharVector
		       structure VectorSlice = CharVectorSlice
			  
		       val chunkSize = Primitive.TextIO.bufSize
		       val fileTypeFlags = [PosixPrimitive.FileSys.O.text]
		       val line = SOME {isLine = fn c => c = #"\n",
					lineElem = #"\n"}
		       val mkReader = Posix.IO.mkTextReader
		       val mkWriter = Posix.IO.mkTextWriter
		       val someElem = (#"\000": Char.char)
		       val xlatePos = SOME {fromInt = fn i => i,
					    toInt = fn i => i})
      open IO

      structure StreamIO =
	 struct
	    open StreamIO

	    fun outputSubstr (s, ss) = outputSlice (s, ss)
	 end

      val outputSubstr = outputSlice

      val openString = openVector

      fun print (s: string) = (output (stdOut, s); flushOut stdOut)
   end

structure TextIOGlobal: TEXT_IO_GLOBAL = TextIO
open TextIOGlobal
