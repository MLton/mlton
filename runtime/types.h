/* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */

/* Can't use _TYPES_H_ because MSVCRT uses it.  So, we use _MLTON_TYPES_H_. */

#ifndef _MLTON_TYPES_H_
#define _MLTON_TYPES_H_

typedef char Int8;
typedef short Int16;
typedef long Int32;
typedef long long Int64;
typedef char *Pointer;
typedef Pointer pointer;
typedef float Real32;
typedef double Real64;
typedef unsigned char Word8;
typedef unsigned short Word16;
typedef unsigned long Word32;
typedef unsigned long long Word64;

typedef Int8 WordS8;
typedef Int16 WordS16;
typedef Int32 WordS32;
typedef Int64 WordS64;

typedef Word8 WordU8;
typedef Word16 WordU16;
typedef Word32 WordU32;
typedef Word64 WordU64;

typedef Int32 Int;
typedef Real64 Real;
typedef Word8 Char;
typedef Word32 Word;
typedef Int64 Position;

typedef Int Bool;
typedef Word Cpointer;
typedef Word Cstring;
typedef Word CstringArray;
typedef Word Dirstream;
typedef Int Fd;
typedef Word Flag;
typedef Word Gid;
typedef Word Mode;
typedef Word NullString;
typedef Int Pid;
typedef Int Resource;
typedef Word Rlimit;
typedef Int Signal;
typedef Int Size;
typedef Int Speed;
typedef Int Ssize;
typedef Int Status;
typedef Int Syserror;
typedef Pointer Thread;
typedef Word Uid;

#endif /* _MLTON_TYPES_H_ */
