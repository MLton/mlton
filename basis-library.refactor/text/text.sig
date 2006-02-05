signature TEXT =
   sig
      structure Char: CHAR
      structure CharArray: MONO_ARRAY
      structure CharArraySlice: MONO_ARRAY_SLICE
      structure CharVector: MONO_VECTOR
      structure CharVectorSlice: MONO_VECTOR_SLICE
      structure String: STRING
      structure Substring: SUBSTRING
      sharing type Char.char
         = CharArray.elem
         = CharArraySlice.elem
         = CharVector.elem
         = CharVectorSlice.elem
         = String.char
         = Substring.char
      sharing type Char.string
         = CharArraySlice.vector
         = CharVector.vector
         = CharArray.vector
         = CharVectorSlice.vector
         = String.string
         = Substring.string
      sharing type CharArray.array = CharArraySlice.array
      sharing type CharVectorSlice.slice = CharArraySlice.vector_slice
   end
