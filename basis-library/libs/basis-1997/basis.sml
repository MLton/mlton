structure Basis1997: BASIS_1997 =
   struct
      open Basis2002
      structure VectorArray = VectorArrayConvert
	                      (structure Vector = Vector
			       structure VectorSlice = VectorSlice
			       structure Array = Array
			       structure ArraySlice = ArraySlice)
      structure Vector = VectorArray.Vector
      structure Array = VectorArray.Array
      structure BoolVectorArray = MonoVectorArrayArray2Convert
	                          (structure Vector = BoolVector
				   structure VectorSlice = BoolVectorSlice
				   structure Array = BoolArray
				   structure ArraySlice = BoolArraySlice
				   structure Array2 = BoolArray2)
      structure BoolVector = BoolVectorArray.Vector
      structure BoolArray = BoolVectorArray.Array
      structure BoolArray2 = BoolVectorArray.Array2
      structure CharVectorArray = MonoVectorArrayArray2Convert
	                          (structure Vector = CharVector
				   structure VectorSlice = CharVectorSlice
				   structure Array = CharArray
				   structure ArraySlice = CharArraySlice
				   structure Array2 = CharArray2)
      structure CharVector = CharVectorArray.Vector
      structure CharArray = CharVectorArray.Array
      structure CharArray2 = CharVectorArray.Array2
      structure IntVectorArray = MonoVectorArrayArray2Convert
	                         (structure Vector = IntVector
				  structure VectorSlice = IntVectorSlice
				  structure Array = IntArray
				  structure ArraySlice = IntArraySlice
				  structure Array2 = IntArray2)
      structure IntVector = IntVectorArray.Vector
      structure IntArray = IntVectorArray.Array
      structure IntArray2 = IntVectorArray.Array2
      structure Int32VectorArray = MonoVectorArrayArray2Convert
	                           (structure Vector = Int32Vector
			 	    structure VectorSlice = Int32VectorSlice
				    structure Array = Int32Array
				    structure ArraySlice = Int32ArraySlice
				    structure Array2 = Int32Array2)
      structure Int32Vector = Int32VectorArray.Vector
      structure Int32Array = Int32VectorArray.Array
      structure Int32Array2 = Int32VectorArray.Array2
      structure RealVectorArray = MonoVectorArrayArray2Convert
	                          (structure Vector = RealVector
				   structure VectorSlice = RealVectorSlice
				   structure Array = RealArray
				   structure ArraySlice = RealArraySlice
				   structure Array2 = RealArray2)
      structure RealVector = RealVectorArray.Vector
      structure RealArray = RealVectorArray.Array
      structure RealArray2 = RealVectorArray.Array2
      structure Real64VectorArray = MonoVectorArrayArray2Convert
	                            (structure Vector = Real64Vector
				     structure VectorSlice = Real64VectorSlice
				     structure Array = Real64Array
				     structure ArraySlice = Real64ArraySlice
				     structure Array2 = Real64Array2)
      structure Real64Vector = Real64VectorArray.Vector
      structure Real64Array = Real64VectorArray.Array
      structure Real64Array2 = Real64VectorArray.Array2
      structure Word8VectorArray = MonoVectorArrayArray2Convert
	                           (structure Vector = Word8Vector
			 	    structure VectorSlice = Word8VectorSlice
				    structure Array = Word8Array
				    structure ArraySlice = Word8ArraySlice
				    structure Array2 = Word8Array2)
      structure Word8Vector = Word8VectorArray.Vector
      structure Word8Array = Word8VectorArray.Array
      structure Word8Array2 = Word8VectorArray.Array2

      structure Text = TextConvert(structure Text = Text)
      structure Char = Text.Char
      structure String = Text.String
      structure Substring = Text.Substring

      structure IEEEReal = IEEERealConvert(structure IEEEReal = IEEEReal)

      structure Posix = PosixConvert(structure Posix = Posix)

      structure OS = OSConvert(structure OS = OS)
      structure Timer = TimerConvert(structure Timer = Timer)

      structure IO = IOConvert(structure IO = IO)
   end
