(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
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
