structure TextPrimIO: PRIM_IO
                      where type array = CharArray.array
		      where type vector = CharVector.vector
		      where type elem = Char.char =
  PrimIO (structure Array = CharArray
	  structure ArraySlice = CharArraySlice
	  structure Vector = CharVector
	  structure VectorSlice = CharVectorSlice
	  type pos = Position.int
	  val compare = Position.compare
	  val someElem = #"\000": Char.char)

