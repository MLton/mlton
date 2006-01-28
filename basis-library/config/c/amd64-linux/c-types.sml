(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure C = struct


(* C *)
structure Char = Int8
structure SChar = Int8
structure UChar = Word8
structure Short = Int16
structure SShort = Int16
structure UShort = Word16
structure Int = Int32
structure SInt = Int32
structure UInt = Word32
structure Long = Int32
structure SLong = Int32
structure ULong = Word32
structure LongLong = Int64
structure SLongLong = Int64
structure ULongLong = Word64
structure Float = Real32
structure Double = Real64
structure Size = Word32

structure String = Word32
structure StringArray = Word32

(* Generic integers *)
structure Fd = Int
structure Signal = Int
structure Status = Int
structure Sock = Int

(* from <dirent.h> *)
structure DirP = Word32

(* from <poll.h> *)
structure NFds = Word32

(* from <resource.h> *)
structure RLim = Word64

(* from <sys/types.h> *)
structure Clock = Int32
structure Dev = Word64
structure GId = Word32
structure Id = Word32
structure INo = Word64
structure Mode = Word32
structure NLink = Word32
structure Off = Int64
structure PId = Int32
structure SSize = Int32
structure SUSeconds = Int32
structure Time = Int32
structure UId = Word32
structure USeconds = Word32

(* from <sys/socket.h> *)
structure Socklen = Word32

(* from <termios.h> *)
structure CC = Word8
structure Speed = Word32
structure TCFlag = Word32

(* from "gmp.h" *)
structure MPLimb = Word32


structure Errno = struct type 'a t = 'a end
end
