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
structure VectorSlice:> VECTOR_SLICE where type 'a vector = 'a Vector.vector = VectorSlice
(* Basis:
VECTOR_SLICE shouldn't include type 'a vector; should reference 'a Vector.vector directly.
*)
(* Basis:
structure VectorSlice: VECTOR_SLICE
*)
structure Array: ARRAY = Array
structure ArraySlice:> ARRAY_SLICE where type 'a array = 'a Array.array
                                     and type 'a vector = 'a Vector.vector
                                     and type 'a vector_slice = 'a VectorSlice.slice = ArraySlice
(* Basis:
ARRAY_SLICE shouldn't include type 'a array, 'a vector, or 'a vector_slice; 
should reference 'a Array.array, 'a Vector.vector, and 'a VectorSlice.slice directly.
*)
(* Basis:
structure ArraySlice: ARRAY_SLICE
*)
structure Array2: ARRAY2 = Array2
structure BinIO:> BIN_IO = BinIO
structure Bool: BOOL = Bool
structure BoolVector:> MONO_VECTOR where type elem = bool = BoolVector
structure BoolArray:> MONO_ARRAY where type vector = BoolVector.vector
                                 where type elem = bool = BoolArray
structure BoolArray2: MONO_ARRAY2 = BoolArray2
structure Byte: BYTE = Byte
structure Char: CHAR = Char
structure CharVector:> MONO_VECTOR where type vector = String.string
				   where type elem = Char.char = CharVector
structure CharArray:> MONO_ARRAY where type vector = CharVector.vector
                                 where type elem = Char.char = CharArray
(* Basis:
structure CharArray:> MONO_ARRAY where type vector = CharVector.vector
                                 where type elem = char = CharArray
*)
structure CharArray2: MONO_ARRAY2 = CharArray2
structure CommandLine: COMMAND_LINE = CommandLine
structure Date: DATE = Date
structure FixedInt:> INTEGER = Int32
structure General: GENERAL = General
structure IEEEReal: IEEE_REAL = IEEEReal
structure Int:> INTEGER = Int
structure IntVector:> MONO_VECTOR where type elem = int = IntVector
structure IntArray: MONO_ARRAY where type vector = IntVector.vector
                               where type elem = int = IntArray
structure IntArray2: MONO_ARRAY2 = IntArray2
structure Int32:> INTEGER = Int32
structure Int32Vector:> MONO_VECTOR where type elem = Int32.int = Int32Vector
structure Int32Array: MONO_ARRAY where type vector = Int32Vector.vector
                                 where type elem = Int32.int = Int32Array
structure IntInf:> INT_INF = IntInf
(* Basis:
structure IntInf: INT_INF = IntInf
*)
structure IO: IO = IO
structure LargeInt:> INTEGER = LargeInt
structure LargeReal:> REAL = LargeReal
structure LargeWord:> WORD = Word
structure List: LIST = List
structure ListPair: LIST_PAIR = ListPair
structure Math: MATH = Real.Math
structure Option: OPTION = Option
structure OS: OS = OS
structure Pack32Big:> PACK_WORD = Pack32Big
structure Pack32Little:> PACK_WORD = Pack32Little
structure Position:> INTEGER = Position
structure Posix: POSIX = Posix
(* Funny shuffle with Real and PackRealLittle to get opaque signatures to work out. *)
structure Real: REAL = Real
structure PackRealLittle:> PACK_REAL where type real = Real.real = PackRealLittle
structure Real:> REAL where type real = PackRealLittle.real = Real
structure RealVector:> MONO_VECTOR where type elem = real = RealVector
structure RealArray:> MONO_ARRAY where type vector = RealVector.vector
                                 where type elem = real = RealArray
(* Funny shuffle with Real64 and PackReal64Little to get opaque signatures to work out. *)
structure Real64: REAL = Real64
structure PackReal64Little:> PACK_REAL where type real = Real64.real = PackReal64Little
structure Real64:> REAL where type real = PackReal64Little.real = Real64
structure Real64Vector:> MONO_VECTOR where type elem = Real64.real = Real64Vector
structure Real64Array:> MONO_ARRAY where type vector = Real64Vector.vector
                                   where type elem = Real64.real = Real64Array
structure SML90:> SML90 = SML90
(* Basis:
(* structure SML90:> SML90 = SML90 *)
*)
structure String: STRING = String
structure StringCvt: STRING_CVT = StringCvt
structure Substring: SUBSTRING = Substring
structure SysWord:> WORD = SysWord
structure TextIO:> TEXT_IO = TextIO
structure Time: TIME = Time
structure Timer:> TIMER = Timer
structure Unix: UNIX = Unix
structure Word:> WORD = Word
structure WordVector:> MONO_VECTOR where type elem = word = WordVector
structure WordArray: MONO_ARRAY where type vector = WordVector.vector
                                where type elem = word = WordArray
structure WordArray2: MONO_ARRAY2 = WordArray2
structure Word8:> WORD = Word8
structure Word8Vector:> MONO_VECTOR where type elem = Word8.word = Word8Vector
structure Word8Array:> MONO_ARRAY where type vector = Word8Vector.vector
                                  where type elem = Word8.word = Word8Array
structure Word8Array2: MONO_ARRAY2 = Word8Array2
structure Word32:> WORD = Word32
structure Word32Vector:> MONO_VECTOR where type elem = Word32.word = Word32Vector
structure Word32Array:> MONO_ARRAY where type vector = Word32Vector.vector
                                   where type elem = Word32.word = Word32Array

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

