
structure TextPrimIO : PRIM_IO
                       where type array = CharArray.array
		       where type vector = CharVector.vector
		       where type elem = Char.char =
  PrimIOExtra(structure Vector = CharVector
	      structure Array = CharArray
	      val someElem = (#"\000": Char.char)
	      type pos = Position.int
	      val compare = Position.compare)
