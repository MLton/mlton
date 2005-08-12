(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Required signatures *)
signature CHAR = CHAR
signature INTEGER = INTEGER
signature MATH = MATH
signature IMPERATIVE_IO = IMPERATIVE_IO
signature MONO_ARRAY = MONO_ARRAY_1997
signature MONO_VECTOR = MONO_VECTOR_1997
signature PRIM_IO = PRIM_IO
signature REAL = REAL_1997
signature STREAM_IO = STREAM_IO
signature STRING = STRING_1997
signature SUBSTRING = SUBSTRING_1997
signature TEXT_IO = TEXT_IO
signature TEXT_STREAM_IO = TEXT_STREAM_IO
signature WORD = WORD_1997

signature ARRAY = ARRAY_1997
signature BIN_IO = BIN_IO
signature BOOL = BOOL
signature BYTE = BYTE
signature COMMAND_LINE = COMMAND_LINE
signature DATE = DATE
signature GENERAL = GENERAL
signature IEEE_REAL = IEEE_REAL_1997
signature IO = IO_1997
signature LIST = LIST
signature LIST_PAIR = LIST_PAIR
signature OPTION = OPTION
signature OS = OS_1997
signature OS_FILE_SYS = OS_FILE_SYS_1997
signature OS_PATH = OS_PATH_1997
signature OS_PROCESS = OS_PROCESS_1997
signature OS_IO = OS_IO
signature SML90 = SML90
signature STRING_CVT = STRING_CVT
signature TIME = TIME
signature TIMER = TIMER_1997
signature VECTOR = VECTOR_1997

(* Optional signatures *)
signature ARRAY2 = ARRAY2
signature INT_INF = INT_INF
(*
signature LOCALE = LOCALE
*)
signature MONO_ARRAY2 = MONO_ARRAY2_1997
(*
signature MULTIBYTE = MULTIBYTE
*)
signature PACK_REAL = PACK_REAL
signature PACK_WORD = PACK_WORD
signature POSIX_FLAGS = POSIX_FLAGS_1997
signature POSIX = POSIX_1997
signature POSIX_ERROR = POSIX_ERROR
signature POSIX_SIGNAL = POSIX_SIGNAL
signature POSIX_PROCESS = POSIX_PROCESS_1997
signature POSIX_PROC_ENV = POSIX_PROC_ENV
signature POSIX_FILE_SYS = POSIX_FILE_SYS_1997
signature POSIX_IO = POSIX_IO_1997
signature POSIX_SYS_DB = POSIX_SYS_DB
signature POSIX_TTY = POSIX_TTY_1997
signature UNIX = UNIX_1997
