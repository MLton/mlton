(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Required signatures *)
signature ARRAY = ARRAY 
signature ARRAY_SLICE = ARRAY_SLICE 
signature BIN_IO = BIN_IO 
signature BOOL = BOOL 
signature BYTE = BYTE 
signature CHAR = CHAR 
signature COMMAND_LINE = COMMAND_LINE 
signature DATE = DATE 
signature GENERAL = GENERAL 
signature IEEE_REAL = IEEE_REAL 
signature IMPERATIVE_IO = IMPERATIVE_IO 
signature INTEGER = INTEGER 
signature INT_INF = INT_INF 
signature IO = IO 
signature LIST = LIST 
signature LIST_PAIR = LIST_PAIR 
signature MATH = MATH 
signature MONO_ARRAY = MONO_ARRAY 
signature MONO_ARRAY_SLICE = MONO_ARRAY_SLICE 
signature MONO_VECTOR = MONO_VECTOR 
signature MONO_VECTOR_SLICE = MONO_VECTOR_SLICE 
signature OPTION = OPTION 
signature OS = OS 
signature OS_FILE_SYS = OS_FILE_SYS 
signature OS_IO = OS_IO 
signature OS_PATH = OS_PATH 
signature OS_PROCESS = OS_PROCESS 
signature PRIM_IO = PRIM_IO 
signature REAL = REAL 
signature STREAM_IO = STREAM_IO
signature STRING = STRING 
signature STRING_CVT = STRING_CVT 
signature SUBSTRING = SUBSTRING 
signature TEXT = TEXT 
signature TEXT_IO = TEXT_IO 
signature TEXT_STREAM_IO = TEXT_STREAM_IO 
signature TIME = TIME 
signature TIMER = TIMER 
signature VECTOR = VECTOR 
signature VECTOR_SLICE = VECTOR_SLICE 
signature WORD = WORD

(* Optional signatures *)
signature ARRAY2 = ARRAY2 
signature BIT_FLAGS = BIT_FLAGS 
signature GENERIC_SOCK = GENERIC_SOCK 
signature INET_SOCK = INET_SOCK 
signature INT_INF = INT_INF 
signature MONO_ARRAY2 = MONO_ARRAY2 
signature NET_HOST_DB = NET_HOST_DB 
signature NET_PROT_DB = NET_PROT_DB 
signature NET_SERV_DB = NET_SERV_DB 
signature PACK_REAL = PACK_REAL 
signature PACK_WORD = PACK_WORD 
signature POSIX = POSIX 
signature POSIX_ERROR = POSIX_ERROR 
signature POSIX_FILE_SYS = POSIX_FILE_SYS 
signature POSIX_IO = POSIX_IO 
signature POSIX_PROC_ENV = POSIX_PROC_ENV 
signature POSIX_PROCESS = POSIX_PROCESS 
signature POSIX_SIGNAL = POSIX_SIGNAL 
signature POSIX_SYS_DB = POSIX_SYS_DB 
signature POSIX_TTY = POSIX_TTY 
signature SOCKET = SOCKET 
signature UNIX = UNIX 
signature UNIX_SOCK = UNIX_SOCK 
(*
signature WINDOWS = WINDOWS
*)

(* Non-standard signatures *)
signature PRIM_IO_ARG = PRIM_IO_ARG
signature STREAM_IO_ARG = STREAM_IO_ARG
signature IMPERATIVE_IO_ARG = IMPERATIVE_IO_ARG
signature SML90 = SML90

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
signature MLTON_MONO_ARRAY = MLTON_MONO_ARRAY
signature MLTON_MONO_VECTOR = MLTON_MONO_VECTOR
signature MLTON_PLATFORM = MLTON_PLATFORM
signature MLTON_POINTER = MLTON_POINTER
signature MLTON_PROC_ENV = MLTON_PROC_ENV
signature MLTON_PROCESS = MLTON_PROCESS
signature MLTON_PROFILE = MLTON_PROFILE
signature MLTON_RANDOM = MLTON_RANDOM
signature MLTON_REAL = MLTON_REAL
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
