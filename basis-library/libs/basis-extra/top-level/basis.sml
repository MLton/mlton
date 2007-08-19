(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BasisExtra :> BASIS_EXTRA = 
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
      structure IO = IO
      structure Int = Int
      structure LargeInt = LargeInt
      structure LargeReal = LargeReal
      structure LargeWord = LargeWord
      structure List = List
      structure ListPair = ListPair
      structure OS = OS
      structure Option = Option
      structure Position = Position
      structure Real = Real
      structure String = String
      structure StringCvt = StringCvt
      structure Substring = Substring
      structure Text = Text
      structure TextIO = TextIO
      structure TextPrimIO = TextPrimIO
      structure Time = Time
      structure Timer = Timer
      structure Vector = Vector
      structure VectorSlice = VectorSlice
      structure Word = Word
      structure Word8 = Word8
      structure Word8Array = Word8Array
      structure Word8Array2 = Word8Array2
      structure Word8ArraySlice = Word8ArraySlice
      structure Word8Vector = Word8Vector
      structure Word8VectorSlice = Word8VectorSlice

      (* Optional structures *)
      structure Array2 = Array2
      structure BoolArray = BoolArray
      structure BoolArray2 = BoolArray2
      structure BoolArraySlice = BoolArraySlice
      structure BoolVector = BoolVector
      structure BoolVectorSlice = BoolVectorSlice
      structure CharArray2 = CharArray2
      structure FixedInt = FixedInt
      structure GenericSock = GenericSock
      structure INetSock = INetSock
      structure Int1 = Int1
      structure Int2 = Int2
      structure Int3 = Int3
      structure Int4 = Int4
      structure Int5 = Int5
      structure Int6 = Int6
      structure Int7 = Int7
      structure Int8 = Int8
      structure Int9 = Int9
      structure Int10 = Int10
      structure Int11 = Int11
      structure Int12 = Int12
      structure Int13 = Int13
      structure Int14 = Int14
      structure Int15 = Int15
      structure Int16 = Int16
      structure Int16 = Int16
      structure Int17 = Int17
      structure Int18 = Int18
      structure Int19 = Int19
      structure Int20 = Int20
      structure Int21 = Int21
      structure Int22 = Int22
      structure Int23 = Int23
      structure Int24 = Int24
      structure Int25 = Int25
      structure Int26 = Int26
      structure Int27 = Int27
      structure Int28 = Int28
      structure Int29 = Int29
      structure Int30 = Int30
      structure Int31 = Int31
      structure Int32 = Int32
      structure Int64 = Int64
      structure IntArray = IntArray
      structure IntArray2 = IntArray2
      structure IntArraySlice = IntArraySlice
      structure IntVector = IntVector
      structure IntVectorSlice = IntVectorSlice
      structure Int8Array = Int8Array
      structure Int8Array2 = Int8Array2
      structure Int8ArraySlice = Int8ArraySlice
      structure Int8Vector = Int8Vector
      structure Int8VectorSlice = Int8VectorSlice
      structure Int16Array = Int16Array
      structure Int16Array2 = Int16Array2
      structure Int16ArraySlice = Int16ArraySlice
      structure Int16Vector = Int16Vector
      structure Int16VectorSlice = Int16VectorSlice
      structure Int32Array = Int32Array
      structure Int32Array2 = Int32Array2
      structure Int32ArraySlice = Int32ArraySlice
      structure Int32Vector = Int32Vector
      structure Int32VectorSlice = Int32VectorSlice
      structure Int64Array = Int64Array
      structure Int64Array2 = Int64Array2
      structure Int64ArraySlice = Int64ArraySlice
      structure Int64Vector = Int64Vector
      structure Int64VectorSlice = Int64VectorSlice
      structure IntInf = IntInf
      structure LargeIntArray = LargeIntArray
      structure LargeIntArray2 = LargeIntArray2
      structure LargeIntArraySlice = LargeIntArraySlice
      structure LargeIntVector = LargeIntVector
      structure LargeIntVectorSlice = LargeIntVectorSlice
      structure LargeRealArray = LargeRealArray
      structure LargeRealArray2 = LargeRealArray2
      structure LargeRealArraySlice = LargeRealArraySlice
      structure LargeRealVector = LargeRealVector
      structure LargeRealVectorSlice = LargeRealVectorSlice
      structure LargeWordArray = LargeWordArray
      structure LargeWordArray2 = LargeWordArray2
      structure LargeWordArraySlice = LargeWordArraySlice
      structure LargeWordVector = LargeWordVector
      structure LargeWordVectorSlice = LargeWordVectorSlice
      structure NetHostDB = NetHostDB
      structure NetProtDB = NetProtDB
      structure NetServDB = NetServDB
      structure PackReal32Big = PackReal32Big
      structure PackReal32Little = PackReal32Little
      structure PackReal64Big = PackReal64Big
      structure PackReal64Little = PackReal64Little
      structure PackRealBig = PackRealBig
      structure PackRealLittle = PackRealLittle
      structure PackWord16Big = PackWord16Big
      structure PackWord16Little = PackWord16Little
      structure PackWord32Big = PackWord32Big
      structure PackWord32Little = PackWord32Little
      structure PackWord64Big = PackWord64Big
      structure PackWord64Little = PackWord64Little
      structure Posix = Posix
      structure Real32 = Real32
      structure Real32Array = Real32Array
      structure Real32Array2 = Real32Array2
      structure Real32ArraySlice = Real32ArraySlice
      structure Real32Vector = Real32Vector
      structure Real32VectorSlice = Real32VectorSlice
      structure Real64 = Real64
      structure Real64Array = Real64Array
      structure Real64Array2 = Real64Array2
      structure Real64ArraySlice = Real64ArraySlice
      structure Real64Vector = Real64Vector
      structure Real64VectorSlice = Real64VectorSlice
      structure RealArray = RealArray
      structure RealArray2 = RealArray2
      structure RealArraySlice = RealArraySlice
      structure RealVector = RealVector
      structure RealVectorSlice = RealVectorSlice
      structure Socket = Socket
      structure SysWord = SysWord
      structure Unix = Unix
      structure UnixSock = UnixSock
      structure WideChar = WideChar
      structure WideCharArray = WideCharArray
      structure WideCharArray2 = WideCharArray2
      structure WideCharArraySlice = WideCharArraySlice
      structure WideCharVector = WideCharVector
      structure WideCharVectorSlice = WideCharVectorSlice
      structure WideString = WideString
      structure WideSubstring = WideSubstring
      structure WideText = WideText
(*
      structure WideTextIO = WideTextIO
      structure WideTextPrimIO = WideTextPrimIO
*)
(*
      structure Windows = Windows
*)
      structure Word1 = Word1
      structure Word2 = Word2
      structure Word3 = Word3
      structure Word4 = Word4
      structure Word5 = Word5
      structure Word6 = Word6
      structure Word7 = Word7
      structure Word8 = Word8
      structure Word9 = Word9
      structure Word10 = Word10
      structure Word11 = Word11
      structure Word12 = Word12
      structure Word13 = Word13
      structure Word14 = Word14
      structure Word15 = Word15
      structure Word16 = Word16
      structure Word17 = Word17
      structure Word18 = Word18
      structure Word19 = Word19
      structure Word20 = Word20
      structure Word21 = Word21
      structure Word22 = Word22
      structure Word23 = Word23
      structure Word24 = Word24
      structure Word25 = Word25
      structure Word26 = Word26
      structure Word27 = Word27
      structure Word28 = Word28
      structure Word29 = Word29
      structure Word30 = Word30
      structure Word31 = Word31
      structure Word32 = Word32
      structure Word64 = Word64
      structure Word16 = Word16
      structure WordArray = WordArray
      structure WordArray2 = WordArray2
      structure WordArraySlice = WordArraySlice
      structure WordVector = WordVector
      structure WordVectorSlice = WordVectorSlice
      structure Word16Array = Word16Array
      structure Word16Array2 = Word16Array2
      structure Word16ArraySlice = Word16ArraySlice
      structure Word16Vector = Word16Vector
      structure Word16VectorSlice = Word16VectorSlice
      structure Word32Array = Word32Array
      structure Word32Array2 = Word32Array2
      structure Word32ArraySlice = Word32ArraySlice
      structure Word32Vector = Word32Vector
      structure Word32VectorSlice = Word32VectorSlice
      structure Word64Array = Word64Array
      structure Word64Array2 = Word64Array2
      structure Word64ArraySlice = Word64ArraySlice
      structure Word64Vector = Word64Vector
      structure Word64VectorSlice = Word64VectorSlice

      (* Non-standard structures *)
      structure SML90 = SML90
      structure MLton = MLton
      structure SMLofNJ = SMLofNJ
      structure Unsafe = Unsafe

      open ArrayGlobal
           BoolGlobal
           CharGlobal
           IntGlobal
           GeneralGlobal
           ListGlobal
           OptionGlobal
           RealGlobal
           StringGlobal
           SubstringGlobal
           TextIOGlobal
           VectorGlobal
           WordGlobal
      val real = real
      val op = = op =
      val op <> = op <>
      val vector = vector
      datatype ref = datatype ref
   end
