signature TEXT =
   sig
      structure Char: CHAR
      structure String: STRING
      structure Substring: SUBSTRING
      structure CharVector: MONO_VECTOR
      structure CharArray: MONO_ARRAY
      sharing type Char.char = String.char = Substring.char
	= CharVector.elem = CharArray.elem
      sharing type Char.string = String.string = Substring.string
	= CharVector.vector = CharArray.vector
   end