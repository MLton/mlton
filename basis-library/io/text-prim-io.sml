
structure TextPrimIO : PRIM_IO
                       where type array = CharArray.array
		       where type vector = CharVector.vector
		       where type elem = Char.char =
  PrimIOExtra(structure A = CharArray
	      structure V = CharVector
	      val someElem = (#"\000": Char.char)
	      type pos = Position.int
	      val compare = Position.compare)
