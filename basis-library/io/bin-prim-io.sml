
structure BinPrimIO : PRIM_IO
                      where type array = Word8Array.array
		      where type vector = Word8Vector.vector
		      where type elem = Word8.word
		      where type pos = Position.int =
  PrimIOExtra(structure A = Word8Array
	      structure V = Word8Vector
	      val someElem = (0wx0: Word8.word)
	      type pos = Position.int
	      val compare = Position.compare)
