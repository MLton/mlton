structure TextIO: TEXT_IO_EXTRA =
   struct
      structure S = struct
		      structure PrimIO = TextPrimIO
		      structure Array = CharArray
		      structure Vector = CharVector
		      val someElem = (#"\000": Char.char)
		      val lineElem = (#"\n": Char.char)
		      fun isLine c = c = lineElem
		      val hasLine = CharVector.exists isLine
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
		      val fileTypeFlags = [PosixPrimitive.FileSys.O.text]
		      val mkReader = Posix.IO.mkTextReader
		      val mkWriter = Posix.IO.mkTextWriter
		    end
      structure ImperativeIO = ImperativeIOExtraFile(open S)
      structure FastImperativeIO = FastImperativeIOExtraFile(open S)
      open FastImperativeIO

      structure StreamIO =
	 struct
	    open SIO
	    val outputSubstr = fn (os, ss) => 
	      let
		val (s, i, sz) = Substring.base ss
	      in
		outputSlice (os, (s, i, SOME sz))
	      end
	 end

      val outputSubstr = fn (os, ss) => 
	let
	  val (s, i, sz) = Substring.base ss
	in
	  outputSlice (os, (s, i, SOME sz))
	end
      val openString = openVector
      fun print (s: string) = (output (stdOut, s); flushOut stdOut)
   end

structure TextIOGlobal: TEXT_IO_GLOBAL = TextIO
open TextIOGlobal
