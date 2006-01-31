(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure C = struct


(* C *)
structure Char = struct open Int8 type t = int end
structure SChar = struct open Int8 type t = int end
structure UChar = struct open Word8 type t = word end
structure Short = struct open Int16 type t = int end
structure SShort = struct open Int16 type t = int end
structure UShort = struct open Word16 type t = word end
structure Int = struct open Int32 type t = int end
structure SInt = struct open Int32 type t = int end
structure UInt = struct open Word32 type t = word end
structure Long = struct open Int32 type t = int end
structure SLong = struct open Int32 type t = int end
structure ULong = struct open Word32 type t = word end
structure LongLong = struct open Int64 type t = int end
structure SLongLong = struct open Int64 type t = int end
structure ULongLong = struct open Word64 type t = word end
structure Float = struct open Real32 type t = real end
structure Double = struct open Real64 type t = real end
structure Size = struct open Word32 type t = word end

structure String = Pointer
structure StringArray = Pointer

(* Generic integers *)
structure Fd = Int
structure Signal = Int
structure Status = Int
structure Sock = Int

(* from <dirent.h> *)
structure DirP = struct open Word32 type t = word end

(* from <poll.h> *)
structure NFds = struct open Word32 type t = word end

(* from <resource.h> *)
structure RLim = struct open Word64 type t = word end

(* from <sys/types.h> *)
structure Clock = struct open Int32 type t = int end
structure Dev = struct open Word64 type t = word end
structure GId = struct open Word32 type t = word end
structure Id = struct open Word32 type t = word end
structure INo = struct open Word64 type t = word end
structure Mode = struct open Word32 type t = word end
structure NLink = struct open Word32 type t = word end
structure Off = struct open Int64 type t = int end
structure PId = struct open Int32 type t = int end
structure SSize = struct open Int32 type t = int end
structure SUSeconds = struct open Int32 type t = int end
structure Time = struct open Int32 type t = int end
structure UId = struct open Word32 type t = word end
structure USeconds = struct open Word32 type t = word end

(* from <sys/socket.h> *)
structure Socklen = struct open Word32 type t = word end

(* from <termios.h> *)
structure CC = struct open Word8 type t = word end
structure Speed = struct open Word32 type t = word end
structure TCFlag = struct open Word32 type t = word end

(* from "gmp.h" *)
structure MPLimb = struct open Word32 type t = word end


structure Errno = struct type 'a t = 'a end
end
