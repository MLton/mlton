structure BinIO: BIN_IO_EXTRA =
   struct
      structure StreamIO = StreamIOExtra(structure PrimIO = BinPrimIO
					 structure Array = Word8Array
					 structure Vector = Word8Vector
					 val someElem = (0wx0: Word8.word))
      structure ImperativeIO = ImperativeIOExtra(structure StreamIO = StreamIO
						 structure Vector = Word8Vector)
      open ImperativeIO

      structure PIO = Posix.IO
      structure PFS = Posix.FileSys
      fun openIn f =
	let 
	  val reader = 
	    PIO.mkBinReader 
	    {fd = PFS.openf(name, PIO.O_RDONLY, PFS.O.flags[])}
	in
	  PIO.mkInstream(StreamIO.mkInstream(reader, empty))
	end
      fun openIn _ = raise (Fail "<unimplemented>")
      fun openOut _ = raise (Fail "<unimplemented>")
      fun openAppend _ = raise (Fail "<unimplemented>")
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