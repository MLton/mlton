(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Non-standard signatures *)
signature MLTON = MLTON
signature MLTON_ARRAY = MLTON_ARRAY
signature MLTON_BIN_IO = MLTON_BIN_IO
signature MLTON_CONT = MLTON_CONT
signature MLTON_EXN = MLTON_EXN
signature MLTON_FINALIZABLE = MLTON_FINALIZABLE
signature MLTON_GC = MLTON_GC
signature MLTON_INT_INF = MLTON_INT_INF
signature MLTON_IO = MLTON_IO
signature MLTON_ITIMER = MLTON_ITIMER
signature MLTON_PLATFORM = MLTON_PLATFORM
signature MLTON_POINTER = MLTON_POINTER
signature MLTON_PROC_ENV = MLTON_PROC_ENV
signature MLTON_PROCESS = MLTON_PROCESS
signature MLTON_PROFILE = MLTON_PROFILE
signature MLTON_RANDOM = MLTON_RANDOM
signature MLTON_RLIMIT = MLTON_RLIMIT
signature MLTON_RUSAGE = MLTON_RUSAGE
signature MLTON_SIGNAL = MLTON_SIGNAL
signature MLTON_SOCKET = MLTON_SOCKET
signature MLTON_SYSLOG = MLTON_SYSLOG
signature MLTON_TEXT_IO = MLTON_TEXT_IO
signature MLTON_THREAD = MLTON_THREAD
signature MLTON_VECTOR = MLTON_VECTOR
signature MLTON_WEAK = MLTON_WEAK
signature MLTON_WORD = MLTON_WORD
signature MLTON_WORLD = MLTON_WORLD
signature SML_OF_NJ = SML_OF_NJ
signature UNSAFE = UNSAFE

(* Quell unused structure warnings. *)
local open Basis1997 BasisNone in end

open Basis2002

val op = = op =

(* Rebind some structures so that their definitions appear later, so that they
 * will be used for displaying tycon names.
 *
 * Order here matters!  Do not alphabetize or otherwise reorder without thinking.
 *)
structure OS = OS
structure BoolArray = BoolArray
structure BoolVector = BoolVector
structure CharArraySlice = CharArraySlice
structure CharArray = CharArray
structure Int8Array = Int8Array
structure Int8Vector = Int8Vector
structure Int16Array = Int16Array
structure Int16Vector = Int16Vector
structure Int32Array = Int32Array
structure Int32Vector = Int32Vector
structure Int64Array = Int64Array
structure Int64Vector = Int64Vector
structure LargeIntArray = LargeIntArray
structure LargeIntVector = LargeIntVector
structure LargeRealArray = LargeRealArray
structure LargeRealVector = LargeRealVector
structure LargeWordArray = LargeWordArray
structure LargeWordVector = LargeWordVector
structure Real32Array = Real32Array
structure Real32Vector = Real32Vector
structure Real64Array = Real64Array
structure Real64Vector = Real64Vector
structure Word8Array = Word8Array
structure Word8Vector = Word8Vector
structure Int8 = Int8
structure Int16 = Int16
structure Int32 = Int32
structure Int64 = Int64
structure IntInf = IntInf
structure Real32 = Real32
structure Real64 = Real64
structure Word8 = Word8
structure Word16 = Word16
structure Word32 = Word32
structure Word64 = Word64

_basis_done MLtonFFI

