structure Basis2002:> BASIS_2002 = 
   struct
      (* Required structures *)
      structure Array = Array
      structure ArraySlice = ArraySlice
      structure BinIO = BinIO
      structure BinPrimIO = BinPrimIO
      structure Bool = Bool
      structure Byte = Byte
      structure Char = Char
      structure CharArray = CharArray
      structure CharArraySlice = CharArraySlice
      structure CharVector = CharVector
      structure CharVectorSlice = CharVectorSlice
      structure CommandLine = CommandLine
      structure Date = Date
      structure General = General
      structure IEEEReal = IEEEReal
      structure Int = Int
      structure IO = IO
      structure LargeInt = LargeInt
      structure LargeReal = LargeReal
      structure LargeWord = LargeWord
      structure List = List
      structure ListPair = ListPair
      structure Math = Math
      structure Option = Option
      structure OS = OS
      structure Position = Position
      structure Real = Real
      structure StringCvt = StringCvt
      structure String = String
      structure Substring = Substring
      structure TextIO = TextIO
      structure TextPrimIO = TextPrimIO
      structure Text = Text
      structure Time = Time
      structure Timer = Timer
      structure VectorSlice = VectorSlice
      structure Vector = Vector
      structure Word8 = Word8
      structure Word8Array = Word8Array
      structure Word8ArraySlice = Word8ArraySlice
      structure Word8Vector = Word8Vector
      structure Word8VectorSlice = Word8VectorSlice
      structure Word8Array2 = Word8Array2
      structure Word = Word

      (* Optional structures *)
      structure Array2 = Array2
      structure BoolArray = BoolArray
      structure BoolArraySlice = BoolArraySlice
      structure BoolVector = BoolVector
      structure BoolVectorSlice = BoolVectorSlice
      structure BoolArray2 = BoolArray2
      structure CharArray2 = CharArray2
      structure FixedInt = FixedInt
      structure GenericSock = GenericSock
      structure INetSock = INetSock
      structure IntArray = Int32Array
      structure IntArraySlice = Int32ArraySlice
      structure IntVector = Int32Vector
      structure IntVectorSlice = Int32VectorSlice
      structure IntArray2 = Int32Array2
      structure Int8 = Int8
      structure Int8Array = Int8Array
      structure Int8ArraySlice = Int8ArraySlice
      structure Int8Vector = Int8Vector
      structure Int8VectorSlice = Int8VectorSlice
      structure Int8Array2 = Int8Array2
      structure Int16 = Int16
      structure Int16Array = Int16Array
      structure Int16ArraySlice = Int16ArraySlice
      structure Int16Vector = Int16Vector
      structure Int16VectorSlice = Int16VectorSlice
      structure Int16Array2 = Int16Array2
      structure Int32 = Int32
      structure Int32Array = Int32Array
      structure Int32ArraySlice = Int32ArraySlice
      structure Int32Vector = Int32Vector
      structure Int32VectorSlice = Int32VectorSlice
      structure Int32Array2 = Int32Array2
      structure Int64 = Int64
      structure Int64Array = Int64Array
      structure Int64ArraySlice = Int64ArraySlice
      structure Int64Vector = Int64Vector
      structure Int64VectorSlice = Int64VectorSlice
      structure Int64Array2 = Int64Array2
      structure IntInf = IntInf
      structure NetHostDB = NetHostDB
      structure NetProtDB = NetProtDB
      structure NetServDB = NetServDB
      structure Pack32Big = Pack32Big
      structure Pack32Little = Pack32Little
      structure PackReal32Big = PackReal32Big
      structure PackReal32Little = PackReal32Little
      structure PackReal64Big = PackReal64Big
      structure PackReal64Little = PackReal64Little
      structure PackRealBig = PackRealBig
      structure PackRealLittle = PackRealLittle
      structure Posix = Posix
      structure RealArray = Real64Array
      structure RealArraySlice = Real64ArraySlice
      structure RealVector = Real64Vector
      structure RealVectorSlice = Real64VectorSlice
      structure RealArray2 = Real64Array2
      structure Real32 = Real32
      structure Real32Array = Real32Array
      structure Real32ArraySlice = Real32ArraySlice
      structure Real32Vector = Real32Vector
      structure Real32VectorSlice = Real32VectorSlice
      structure Real32Array2 = Real32Array2
      structure Real64 = Real64
      structure Real64Array = Real64Array
      structure Real64ArraySlice = Real64ArraySlice
      structure Real64Vector = Real64Vector
      structure Real64VectorSlice = Real64VectorSlice
      structure Real64Array2 = Real64Array2
      structure Socket = Socket
      structure SysWord = SysWord
      structure UnixSock = UnixSock
      structure Unix = Unix
(*
      structure WideChar = WideChar
      structure WideCharArray = WideCharArray
      structure WideCharArraySlice = WideCharArraySlice
      structure WideCharVector = WideCharVector
      structure WideCharVectorSlice = WideCharVectorSlice
      structure WideCharArray2 = WideCharArray2
      structure WideString = WideString
      structure WideSubstring = WideSubstring
      structure WideTextPrimIO = WideTextPrimIO
      structure WideText = WideText
*)
(*
      structure Windows = Windows
*)
      structure Word16 = Word16
      structure Word16Array = Word16Array
      structure Word16ArraySlice = Word16ArraySlice
      structure Word16Vector = Word16Vector
      structure Word16VectorSlice = Word16VectorSlice
      structure Word16Array2 = Word16Array2
      structure Word32 = Word32
      structure Word32Array = Word32Array
      structure Word32ArraySlice = Word32ArraySlice
      structure Word32Vector = Word32Vector
      structure Word32VectorSlice = Word32VectorSlice
      structure Word32Array2 = Word32Array2

      open ArrayGlobal
	   BoolGlobal
	   CharGlobal
	   IntGlobal
	   GeneralGlobal
	   ListGlobal
	   OptionGlobal
	   RealGlobal
	   StringGlobal
	   RealGlobal
	   SubstringGlobal
	   TextIOGlobal
	   VectorGlobal
	   WordGlobal
      val real = real
      val op <> = op <>
      val vector = vector
      datatype ref = datatype ref
   end
