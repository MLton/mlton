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
signature WORD = WORD
   
structure Array: ARRAY = Array
structure Array2: ARRAY2 = Array2
structure BinIO:> BIN_IO = BinIO
structure Bool: BOOL = Bool
structure BoolArray: MONO_ARRAY = BoolArray
structure BoolArray2: MONO_ARRAY2 = BoolArray2
structure BoolVector: MONO_VECTOR = BoolVector
structure Byte: BYTE = Byte
structure Char: CHAR = Char
structure CharArray: MONO_ARRAY = CharArray
structure CharArray2: MONO_ARRAY2 = CharArray2
structure CharVector: MONO_VECTOR = CharVector
structure CommandLine: COMMAND_LINE = CommandLine
structure Date: DATE = Date
structure FixedInt: INTEGER = Int32
structure General: GENERAL = General
structure IEEEReal: IEEE_REAL = IEEEReal
structure Int: INTEGER = Int
structure Int32: INTEGER = Int32
structure IntArray: MONO_ARRAY = IntArray
structure IntArray2: MONO_ARRAY2 = IntArray2
structure IntVector: MONO_VECTOR = IntVector
structure IntInf: INT_INF = IntInf
structure IO: IO = IO
structure LargeInt: INTEGER = LargeInt
structure LargeReal: REAL = LargeReal
structure LargeWord: WORD = Word
structure List: LIST = List
structure ListPair: LIST_PAIR = ListPair
structure Math: MATH = Real.Math
structure Option: OPTION = Option
structure OS: OS = OS
structure PackRealLittle: PACK_REAL = PackReal64Little
structure PackReal64Little: PACK_REAL = PackReal64Little
structure Pack32Big: PACK_WORD = Pack32Big
structure Pack32Little: PACK_WORD = Pack32Little
structure Position: INTEGER = Position
structure Posix: POSIX = Posix
structure Real: REAL = Real
structure RealArray: MONO_ARRAY = RealArray
structure Real64Array: MONO_ARRAY = RealArray
structure RealArray2: MONO_ARRAY2 = RealArray2
structure RealVector: MONO_VECTOR = RealVector
structure SML90:> SML90 = SML90
structure String: STRING = String
structure StringCvt: STRING_CVT = StringCvt
structure Substring: SUBSTRING = Substring
structure SysWord: WORD = SysWord
structure TextIO:> TEXT_IO = TextIO
structure Time: TIME = Time
structure Timer:> TIMER = Timer
structure Unix: UNIX = Unix
structure Vector: VECTOR = Vector
structure Word: WORD = Word
structure Word8: WORD = Word8
structure Word8Array: MONO_ARRAY = Word8Array
structure Word8Array2: MONO_ARRAY2 = Word8Array2
structure Word8Vector: MONO_VECTOR = Word8Vector
structure Word32: WORD = Word32

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

