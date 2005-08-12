/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* Can't use _TYPES_H_ because MSVCRT uses it.  So, we use _MLTON_TYPES_H_. */

#ifndef _MLTON_TYPES_H_
#define _MLTON_TYPES_H_

/* We need these because in header files for exported SML functions, types.h is
 * included without platform.h.
 */
#ifndef _ISOC99_SOURCE
#define _ISOC99_SOURCE
#endif
#if (defined (__OpenBSD__))
#include <inttypes.h>
#elif (defined (__sun__))
#include <sys/int_types.h>
#else
#include <stdint.h>
#endif

typedef int8_t Int8;
typedef int16_t Int16;
typedef int32_t Int32;
typedef int64_t Int64;
typedef char *Pointer;
typedef Pointer pointer;
typedef float Real32;
typedef double Real64;
typedef uint8_t Word8;
typedef uint16_t Word16;
typedef uint32_t Word32;
typedef uint64_t Word64;

typedef Int8 WordS8;
typedef Int16 WordS16;
typedef Int32 WordS32;
typedef Int64 WordS64;

typedef Word8 WordU8;
typedef Word16 WordU16;
typedef Word32 WordU32;
typedef Word64 WordU64;

/* !!! this stuff is all wrong: */
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
