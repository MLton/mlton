structure BinIO: BIN_IO_EXTRA =
   struct
      structure StreamIO = 
	StreamIOExtraFile(structure PrimIO = BinPrimIO
			  structure Array = Word8Array
			  structure Vector = Word8Vector
			  val someElem = (0wx0: Word8.word)
			  val lineElem = (0wx0: Word8.word)
			  fun isLine _ = false
			  fun hasLine _ = false
			  structure Cleaner = Cleaner)
      structure SIO = StreamIO
      structure ImperativeIO = 
	ImperativeIOExtraFile(structure StreamIO = StreamIO
			      structure Vector = Word8Vector
			      structure Array = Word8Array
			      val chunkSize = Primitive.TextIO.bufSize
			      val fileTypeFlags = [PosixPrimitive.FileSys.O.binary]
			      val mkReader = Posix.IO.mkBinReader
			      val mkWriter = Posix.IO.mkBinWriter)
      open ImperativeIO
	
      structure StreamIO =
	struct
	  open SIO
	end

   end

(*
structure BinIO: BIN_IO_EXTRA =
   BinOrTextIO
   (val fileTypeFlags = [PosixPrimitive.FileSys.O.binary]
    structure Cleaner = Cleaner
    structure Int = Int
    structure NativeVector =
       struct
	  type vector = Word8Vector.vector
	  type elem = Word8Vector.elem

	  (* This is already binary *)
	  fun toByte e = e
	  fun fromByte v = v
	  fun fromWord8Vector v = v
	  fun toWord8Vector v = v

	  val concat = Word8Vector.concat
	  val empty = Word8Vector.tabulate (0, fn _ => 0w0)
	  fun isEmpty v = Word8Vector.length v = 0
	  (* No linefeeds in a binary file. *)
	  fun hasLine v = false
	  fun isLine e = false
       end
    structure Primitive = Primitive
    structure String = String)

*)