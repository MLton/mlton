(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ARRAY = ARRAY
signature ARRAY2 = ARRAY2
signature BIN_IO = BIN_IO
signature BIN_STREAM_IO = BIN_STREAM_IO
signature BOOL = BOOL
signature BYTE = BYTE
signature CHAR = CHAR
signature COMMAND_LINE = COMMAND_LINE
signature DATE = DATE
signature GENERAL = GENERAL
signature IEEE_REAL = IEEE_REAL
signature INTEGER = INTEGER
signature INT_INF = INT_INF
signature IO = IO
signature LIST = LIST
signature LIST_PAIR = LIST_PAIR
signature MATH = MATH
signature MONO_ARRAY = MONO_ARRAY
signature MONO_ARRAY2 = MONO_ARRAY2
signature MONO_VECTOR = MONO_VECTOR
signature OPTION = OPTION
signature OS = OS
signature OS_FILE_SYS = OS_FILE_SYS
signature OS_IO = OS_IO
signature OS_PATH = OS_PATH
signature OS_PROCESS = OS_PROCESS
signature PACK_REAL = PACK_REAL
signature PACK_WORD = PACK_WORD
signature POSIX = POSIX
signature POSIX_ERROR = POSIX_ERROR
signature POSIX_FILESYS = POSIX_FILESYS
signature POSIX_SIGNAL = POSIX_SIGNAL
signature POSIX_FLAGS = POSIX_FLAGS   
signature POSIX_IO = POSIX_IO
signature POSIX_PROCESS = POSIX_PROCESS
signature POSIX_PROC_ENV = POSIX_PROC_ENV
signature POSIX_SIGNAL = POSIX_SIGNAL
signature POSIX_SYS_DB = POSIX_SYS_DB
signature POSIX_TTY = POSIX_TTY
signature REAL = REAL
signature SML90 = SML90
signature STREAM_IO = STREAM_IO
signature STRING = STRING
signature STRING_CVT = STRING_CVT
signature SUBSTRING = SUBSTRING
signature TEXT_IO = TEXT_IO
signature TEXT_STREAM_IO = TEXT_STREAM_IO
signature TIME = TIME
signature TIMER = TIMER
signature UNIX = UNIX
signature VECTOR = VECTOR
signature VECTOR_SLICE = VECTOR_SLICE
signature WORD = WORD
   
structure Vector: VECTOR = Vector
structure VectorSlice: VECTOR_SLICE where type 'a vector = 'a Vector.vector = VectorSlice
(* Basis:
VECTOR_SLICE shouldn't include type 'a vector; should reference 'a Vector.vector directly.
*)
structure Array: ARRAY = Array
structure ArraySlice: ARRAY_SLICE where type 'a array = 'a Array.array
                                    and type 'a vector = 'a Vector.vector
                                    and type 'a vector_slice = 'a VectorSlice.slice = ArraySlice
(* Basis:
ARRAY_SLICE shouldn't include type 'a array, 'a vector, 'a vector_slice; 
should reference 'a Array.array, 'a Vector.vector, 'a VectorSlice.slice directly.
*)
structure Array2: ARRAY2 = Array2
structure BinIO:> BIN_IO = BinIO
structure Bool: BOOL = Bool
structure BoolVector: MONO_VECTOR where type elem = Bool.bool = BoolVector
structure BoolVectorSlice: MONO_VECTOR_SLICE where type vector = BoolVector.vector
                                             where type elem = Bool.bool = BoolVectorSlice
structure BoolArray: MONO_ARRAY where type vector = BoolVector.vector
                                where type elem = Bool.bool = BoolArray
structure BoolArraySlice: MONO_ARRAY_SLICE where type vector = BoolVector.vector
                                           where type vector_slice = BoolVectorSlice.slice
                                           where type array = BoolArray.array
                                           where type elem = Bool.bool = BoolArraySlice
structure BoolArray2: MONO_ARRAY2 where type vector = BoolVector.vector
                                  where type elem = Bool.bool = BoolArray2
(* Basis:
structure Bool: BOOL
structure BoolVector:> MONO_VECTOR 
                       where type elem = bool
structure BoolVectorSlice:> MONO_VECTOR_SLICE 
                            where type vector = BoolVector.vector
                            where type elem = bool
structure BoolArray:> MONO_ARRAY 
                      where type vector = BoolVector.vector
                      where type elem = bool
structure BoolArraySlice:> MONO_ARRAY_SLICE 
                           where type vector = BoolVector.vector
                           where type vector_slice = BoolVectorSlice.slice
                           where type array = BoolArray.array
                           where type elem = bool
structure BoolArray2:> MONO_ARRAY2 
                       where type vector = BoolVector.vector
                       where type bool = bool
*)
structure Byte: BYTE = Byte
structure Char: CHAR = Char
structure String: STRING where type string = CharVector.vector
                         where type char = Char.char = String
structure Substring: SUBSTRING where type substring = CharVectorSlice.slice
                               where type string = String.string
                               where type char = Char.char = Substring
structure CharVector: MONO_VECTOR where type vector = String.string
                                  where type elem = Char.char = CharVector
structure CharVectorSlice: MONO_VECTOR_SLICE where type vector = String.string
                                             where type elem = Char.char 
                                             where type slice = Substring.substring = CharVectorSlice
structure CharArray: MONO_ARRAY where type vector = CharVector.vector
                                where type elem = Char.char = CharArray
structure CharArraySlice: MONO_ARRAY_SLICE where type vector = CharVector.vector
                                           where type vector_slice = CharVectorSlice.slice
                                           where type array = CharArray.array
                                           where type elem = Char.char = CharArraySlice
structure CharArray2: MONO_ARRAY2 where type vector = CharVector.vector
                                  where type elem = Char.char = CharArray2
structure Text: TEXT = Text
(* Basis:
structure Char:> CHAR
structure String:> STRING where type string = CharVector.vector
                          where type char = Char.char
structure Substring:> SUBSTRING where type substring = CharVectorSlice.slice
                                where type string = String.string
                                where type char = Char.char
structure CharVector:> MONO_VECTOR where type vector = String.string
				   where type elem = Char.char
structure CharVectorSlice:> MONO_VECTOR_SLICE where type slice = Substring.substring
                                              where type vector = String.string
                                              where type elem = Char.char
structure CharArray:> MONO_ARRAY where type vector = CharVector.vector
                                 where type elem = char
structure CharArraySlice:> MONO_ARRAY_SLICE where type vector = CharVector.vector
                                            where type vector_slice = CharVectorSlice.slice
                                            where type array = CharVector.vector
                                            where type elem = char
structure CharArray2:> MONO_ARRAY2 where type vector = CharVector.vector
                                   where type elem = char
structure Text:> TEXT = Text
*)
structure CommandLine: COMMAND_LINE = CommandLine
structure Date: DATE = Date
structure FixedInt:> INTEGER = Int32
structure General: GENERAL = General
structure IEEEReal: IEEE_REAL = IEEEReal
structure Int: INTEGER = Int
structure IntVector: MONO_VECTOR where type elem = Int.int = IntVector
structure IntVectorSlice: MONO_VECTOR_SLICE where type vector = IntVector.vector
                                            where type elem = Int.int = IntVectorSlice
structure IntArray: MONO_ARRAY where type vector = IntVector.vector
                               where type elem = Int.int = IntArray
structure IntArraySlice: MONO_ARRAY_SLICE where type vector = IntVector.vector
                                          where type vector_slice = IntVectorSlice.slice
                                          where type array = IntArray.array
                                          where type elem = Int.int = IntArraySlice
structure IntArray2: MONO_ARRAY2 where type vector = IntVector.vector
                                 where type elem = Int.int = IntArray2
(* Basis:
structure Int:> INTEGER
structure IntVector:> MONO_VECTOR 
                      where type elem = int
structure IntVectorSlice:> MONO_VECTOR_SLICE
                           where type vector = IntVector.vector
                           where type elem = int
structure IntArray:> MONO_ARRAY 
                     where type vector = IntVector.vector
                     where type elem = int
structure IntArraySlice:> MONO_ARRAY_SLICE
                          where type vector = IntVector.vector
                          where type vector_slice = IntVectorSlice.slice
                          where type array = IntArray.array
                          where type elem = int
structure IntArray2:> MONO_ARRAY2 
                      where type vector = IntVector.vector
                      where type elem = int
*)
structure Int32: INTEGER = Int32
structure Int32Vector: MONO_VECTOR where type elem = Int32.int = Int32Vector
structure Int32VectorSlice: MONO_VECTOR_SLICE where type vector = Int32Vector.vector
                                              where type elem = Int32.int = Int32VectorSlice
structure Int32Array: MONO_ARRAY where type vector = Int32Vector.vector
                                 where type elem = Int32.int = Int32Array
structure Int32Array2: MONO_ARRAY2 where type vector = Int32Vector.vector
                                   where type elem = Int32.int = Int32Array2
structure Int32ArraySlice: MONO_ARRAY_SLICE where type vector = Int32Vector.vector
                                            where type vector_slice = Int32VectorSlice.slice
                                            where type array = Int32Array.array
                                            where type elem = Int32.int = Int32ArraySlice
(* Basis:
structure Int32:> INTEGER
structure Int32Vector:> MONO_VECTOR where type elem = Int32.int
structure Int<N>VectorSlice :> MONO_VECTOR_SLICE  (* OPTIONAL *)
  where type elem = Int{N}.int
  where type vector = Int{N}Vector.vector
structure Int32Array:> MONO_ARRAY where type vector = Int32Vector.vector
                                  where type elem = Int32.int
structure Int32Array2:> MONO_ARRAY2 where type vector = Int32Vector.vector
                                    where type elem = Int32.int
structure Int<N>ArraySlice :> MONO_ARRAY_SLICE  (* OPTIONAL *)
  where type vector = Int{N}Vector.vector
  where type vector_slice = Int{N}VectorSlice.slice
  where type array = Int{N}Array.array
  where type elem = Int{N}.int
*)
structure IntInf:> INT_INF = IntInf
(* Basis:
structure IntInf: INT_INF = IntInf
*)
structure IO: IO = IO
structure LargeInt:> INTEGER = LargeInt
structure LargeReal:> REAL = LargeReal
structure LargeWord:> WORD = LargeWord
structure List: LIST = List
structure ListPair: LIST_PAIR = ListPair
structure Option: OPTION = Option
structure OS: OS = OS
structure Pack32Big:> PACK_WORD = Pack32Big
structure Pack32Little:> PACK_WORD = Pack32Little
structure Position:> INTEGER = Position
structure Posix: POSIX = Posix
structure Real: REAL = Real
structure Math: MATH where type real = Real.real = Real.Math
structure PackRealLittle: PACK_REAL where type real = Real.real = PackRealLittle
structure RealVector: MONO_VECTOR where type elem = Real.real = RealVector
structure RealVectorSlice: MONO_VECTOR_SLICE where type vector = RealVector.vector
                                             where type elem = Real.real = RealVectorSlice
structure RealArray: MONO_ARRAY where type vector = RealVector.vector
                                where type elem = Real.real = RealArray
structure RealArraySlice: MONO_ARRAY_SLICE where type vector = RealVector.vector
                                           where type vector_slice = RealVectorSlice.slice
                                           where type array = RealArray.array
                                           where type elem = Real.real = RealArraySlice
structure RealArray2: MONO_ARRAY2 where type vector = RealVector.vector
                                  where type elem = Real.real = RealArray2
(* Basis:
structure Real:> REAL
structure Math:> MATH
structure PackRealLittle:> PACK_REAL where type real = Real.real
structure RealVector:> MONO_VECTOR where type elem = real
structure RealVectorSlice:> MONO_VECTOR_SLICE where type vector = RealVector.vector
                                              where type elem = real
structure RealArray:> MONO_ARRAY where type vector = RealVector.vector
                                 where type elem = real
structure RealArraySlice:> MONO_ARRAY_SLICE where type vector = RealVector.vector
                                            where type vector_slice = RealVectorSlice.slice
                                            where type array = RealArray.array
                                            where type elem = real
structure RealArray2:> MONO_ARRAY2 where type vector = RealVector.vector
                                   where type elem = real
*)
structure Real64: REAL = Real64
structure PackReal64Little: PACK_REAL where type real = Real64.real = PackReal64Little
structure Real64Vector: MONO_VECTOR where type elem = Real64.real = Real64Vector
structure Real64VectorSlice: MONO_VECTOR_SLICE where type vector = Real64Vector.vector
                                               where type elem = Real64.real = Real64VectorSlice
structure Real64Array: MONO_ARRAY where type vector = Real64Vector.vector
                                  where type elem = Real64.real = Real64Array
structure Real64ArraySlice: MONO_ARRAY_SLICE where type vector = Real64Vector.vector
                                             where type vector_slice = Real64VectorSlice.slice
                                             where type array = Real64Array.array
                                             where type elem = Real64.real = Real64ArraySlice
structure Real64Array2: MONO_ARRAY2 where type vector = Real64Vector.vector
                                    where type elem = Real64.real = Real64Array2
(* Basis:
structure Real64:> REAL
structure PackReal64Little:> PACK_REAL where type real = Real64.real
structure Real64Vector:> MONO_VECTOR where type elem = Real64.real
structure Real64VectorSlice:> MONO_VECTOR_SLICE where type elem = Real64.real
                                                where type vector = Real64Vector.vector
structure Real64Array:> MONO_ARRAY where type vector = Real64Vector.vector
                                   where type elem = Real64.real
structure Real64ArraySlice:> MONO_ARRAY_SLICE where type vector = Real64Vector.vector
                                              where type vector_slice = Real64VectorSlice.slice
                                              where type array = Real64Array.array
                                              where type elem = Real64.real
structure Real64Array2:> MONO_ARRAY2 where type vector = Real64Vector.vector
                                     where type elem = Real64.real
*)
structure SML90:> SML90 = SML90
(* Basis:
(* structure SML90:> SML90 = SML90 *)
*)
structure StringCvt: STRING_CVT = StringCvt
structure SysWord:> WORD = SysWord
structure TextIO:> TEXT_IO = TextIO
structure Time: TIME = Time
structure Timer:> TIMER = Timer
structure Unix: UNIX = Unix
structure Word: WORD = Word
structure WordVector: MONO_VECTOR where type elem = Word.word = WordVector
structure WordVectorSlice: MONO_VECTOR_SLICE where type vector = WordVector.vector
                                             where type elem = Word.word = WordVectorSlice
structure WordArray: MONO_ARRAY where type vector = WordVector.vector
                                where type elem = Word.word = WordArray
structure WordArraySlice: MONO_ARRAY_SLICE where type vector = WordVector.vector
                                           where type vector_slice = WordVectorSlice.slice
                                           where type array = WordArray.array
                                           where type elem = Word.word = WordArraySlice
structure WordArray2: MONO_ARRAY2 where type vector = WordVector.vector
                                  where type elem = Word.word = WordArray2
(* Basis: Doesn't actually have Word{Vector[Slice],Array[Slice],Array2} structures
structure Word:> WORD
structure WordVector:> MONO_VECTOR where type elem = word
structure WordVectorSlice:> MONO_VECTOR_SLICE where type elem = word
                                              where type vector = WordVector.vector
structure WordArray:> MONO_ARRAY where type vector = WordVector.vector
                                 where type elem = word
structure WordArraySlice:> MONO_ARRAY_SLICE where type vector = WordVector.vector
                                            where type vector_slice = WordVectorSlice.slice
                                            where type array = WordArray.array
                                            where type elem = word
structure WordArray2:> MONO_ARRAY2 where type vector = WordVector.vector
                                   where type elem = word
*)
structure Word8: WORD = Word8
structure Word8Vector: MONO_VECTOR where type elem = Word8.word = Word8Vector
structure Word8VectorSlice: MONO_VECTOR_SLICE where type vector = Word8Vector.vector
                                              where type elem = Word8.word = Word8VectorSlice
structure Word8Array: MONO_ARRAY where type vector = Word8Vector.vector
                                 where type elem = Word8.word = Word8Array
structure Word8ArraySlice: MONO_ARRAY_SLICE where type vector = Word8Vector.vector
                                            where type vector_slice = Word8VectorSlice.slice
                                            where type array = Word8Array.array
                                            where type elem = Word8.word = Word8ArraySlice
structure Word8Array2: MONO_ARRAY2 where type vector = Word8Vector.vector
                                   where type elem = Word8.word = Word8Array2
(* Basis: 
structure Word8:> WORD
structure Word8Vector:> MONO_VECTOR where type elem = Word8.word
structure Word8VectorSlice:> MONO_VECTOR_SLICE where type elem = Word8.word
                                               where type vector = Word8Vector.vector
structure Word8Array:> MONO_ARRAY where type vector = Word8Vector.vector
                                  where type elem = Word8.word
structure Word8ArraySlice:> MONO_ARRAY_SLICE where type vector = Word8Vector.vector
                                             where type vector_slice = Word8VectorSlice.slice
                                             where type array = Word8Array.array
                                             where type elem = Word8.word
structure Word8Array2:> MONO_ARRAY2 where type vector = Word8Vector.vector
                                    where type elem = Word8.word
*)
structure Word32: WORD = Word32
structure Word32Vector: MONO_VECTOR where type elem = Word32.word = Word32Vector
structure Word32VectorSlice: MONO_VECTOR_SLICE where type vector = Word32Vector.vector
                                               where type elem = Word32.word = Word32VectorSlice
structure Word32Array: MONO_ARRAY where type vector = Word32Vector.vector
                                  where type elem = Word32.word = Word32Array
structure Word32ArraySlice: MONO_ARRAY_SLICE where type vector = Word32Vector.vector
                                             where type vector_slice = Word32VectorSlice.slice
                                             where type array = Word32Array.array
                                             where type elem = Word32.word = Word32ArraySlice
structure Word32Array2: MONO_ARRAY2 where type vector = Word32Vector.vector
                                    where type elem = Word32.word = Word32Array2
(* Basis: 
structure Word32:> WORD
structure Word32Vector:> MONO_VECTOR where type elem = Word32.word
structure Word32VectorSlice:> MONO_VECTOR_SLICE where type elem = Word32.word
                                                where type vector = Word32Vector.vector
structure Word32Array:> MONO_ARRAY where type vector = Word32Vector.vector
                                   where type elem = Word32.word
structure Word32ArraySlice:> MONO_ARRAY_SLICE where type vector = Word32Vector.vector
                                              where type vector_slice = Word32VectorSlice.slice
                                              where type array = Word32Array.array
                                              where type elem = Word32.word
structure Word32Array2:> MONO_ARRAY2 where type vector = Word32Vector.vector
                                     where type elem = Word32.word
*)

open
   ArrayGlobal
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

local
   structure Top: sig
		     val real: int -> real
		     val <> : ''a * ''a -> bool
		     val vector: 'a list -> 'a vector
		  end =
		  struct
		     val real = real
		     val op <> = op <>
		     val vector = vector
		  end
in
   open Top
end

datatype ref = datatype ref

(*------------------------------------*)
(*            nonstandard             *)
(*------------------------------------*)

signature MLTON_CONT = MLTON_CONT
signature MLTON_GC = MLTON_GC
signature MLTON_INT_INF = MLTON_INT_INF
signature MLTON_ITIMER = MLTON_ITIMER
signature MLTON_PROFILE = MLTON_PROFILE
signature MLTON_PTRACE = MLTON_PTRACE
signature MLTON_SIGNAL = MLTON_SIGNAL
signature MLTON_SOCKET = MLTON_SOCKET
signature MLTON_THREAD = MLTON_THREAD
signature MLTON_TEXT_IO = MLTON_TEXT_IO
signature MLTON_WORLD = MLTON_WORLD
structure MLton: MLTON = MLton

structure Primitive = Primitive
   
signature SML_OF_NJ = SML_OF_NJ
structure SMLofNJ: SML_OF_NJ = SMLofNJ

structure Unsafe: UNSAFE = Unsafe

