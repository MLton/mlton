(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Non-standard signatures *)
signature MLTON_ARRAY = MLTON_ARRAY
signature MLTON_BIN_IO = MLTON_BIN_IO
signature MLTON_CONT = MLTON_CONT
signature MLTON_EXN = MLTON_EXN
signature MLTON_FFI = MLTON_FFI
signature MLTON_FINALIZABLE = MLTON_FINALIZABLE
signature MLTON_GC = MLTON_GC
signature MLTON_INT_INF = MLTON_INT_INF
signature MLTON_IO = MLTON_IO
signature MLTON_ITIMER = MLTON_ITIMER
signature MLTON = MLTON
signature MLTON_PLATFORM = MLTON_PLATFORM
signature MLTON_PROC_ENV = MLTON_PROC_ENV
signature MLTON_PROCESS = MLTON_PROCESS
signature MLTON_PROFILE = MLTON_PROFILE
signature MLTON_PTRACE = MLTON_PTRACE
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
signature SML90 = SML90

(* Non-standard structures *)
structure Primitive = Primitive
structure Basis1997 = Basis1997
structure MLton = MLton
structure SMLofNJ = SMLofNJ
structure Unsafe = Unsafe
structure SML90 = SML90

open Basis2002
