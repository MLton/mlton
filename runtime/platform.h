/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_PLATFORM_H_
#define _MLTON_PLATFORM_H_

#include "cenv.h"
#include "util.h"
#include "gc.h"

#if (defined (__APPLE_CC__))
#define __Darwin__
#endif

#if (defined (__CYGWIN__))
#include "platform/cygwin.h"
#elif (defined (__Darwin__))
#include "platform/darwin.h"
#elif (defined (__FreeBSD__))
#include "platform/freebsd.h"
#elif (defined (__linux__))
#include "platform/linux.h"
#elif (defined (__MINGW32__))
#include "platform/mingw.h"
#elif (defined (__NetBSD__))
#include "platform/netbsd.h"
#elif (defined (__OpenBSD__))
#include "platform/openbsd.h"
#elif (defined (__sun__))
#include "platform/solaris.h"
#else
#error unknown platform
#endif

#ifndef MLton_Platform_OS_host
#error MLton_Platform_OS_host not defined
#endif

#ifndef HAS_FPCLASSIFY
#error HAS_FPCLASSIFY not defined
#endif

#ifndef HAS_FEROUND
#error HAS_FEROUND not defined
#endif

#ifndef HAS_REMAP
#error HAS_REMAP not defined
#endif

#ifndef HAS_SIGALTSTACK
#error HAS_SIGALTSTACK not defined
#endif

#ifndef HAS_SIGNBIT
#error HAS_SIGNBIT not defined
#endif

#ifndef HAS_SPAWN
#error HAS_SPAWN not defined
#endif

#ifndef HAS_TIME_PROFILING
#error HAS_TIME_PROFILING not defined
#endif

#ifndef EXECVP
#define EXECVP execvp
#endif

#ifndef EXECVE
#define EXECVE execve
#endif

#if not HAS_FPCLASSIFY
#ifndef FP_INFINITE
#define FP_INFINITE 1
#endif
#ifndef FP_NAN
#define FP_NAN 0
#endif
#ifndef FP_NORMAL
#define FP_NORMAL 4
#endif
#ifndef FP_SUBNORMAL
#define FP_SUBNORMAL 3
#endif
#ifndef FP_ZERO
#define FP_ZERO 2
#endif
#endif

#ifndef SPAWN_MODE
#define SPAWN_MODE 0
#endif

#include "types.h"
#include "basis-ffi.h"

/* ---------------------------------------------------------------- */
/*                        Runtime Init/Exit                         */
/* ---------------------------------------------------------------- */

void MLton_init (int argc, char **argv, GC_state s);
void MLton_exit (GC_state s, Int status) __attribute__ ((noreturn));

/* ---------------------------------------------------------------- */
/*                        Utility libraries                         */
/* ---------------------------------------------------------------- */

int mkdir2 (const char *pathname, mode_t mode);

/* ---------------------------------------------------------------- */
/*                        Garbage Collector                         */
/* ---------------------------------------------------------------- */

/* ------------------------------------------------- */
/*                Virtual Memory                     */
/* ------------------------------------------------- */

/* GC_displayMem displays the virtual memory mapping to stdout.  
 * It is used to diagnose memory problems. 
 */
void GC_displayMem (void);

void *GC_mmapAnon (void *start, size_t length);
void *GC_mmapAnon_safe (void *start, size_t length);
void *GC_mmapAnon_safe_protect (void *start, size_t length, 
                                size_t dead_low, size_t dead_high);
void *GC_mremap (void *start, size_t oldLength, size_t newLength);
void GC_release (void *base, size_t length);
void GC_decommit (void *base, size_t length);

size_t GC_pageSize (void);
size_t GC_totalRam (void);
size_t GC_availRam (void);

void GC_setCygwinUseMmap (bool b);

/* ------------------------------------------------- */
/*                Text Segment                       */
/* ------------------------------------------------- */

void *GC_getTextEnd (void);
void *GC_getTextStart (void);

/* ------------------------------------------------- */
/*                SigProf Handler                    */
/* ------------------------------------------------- */

void GC_setSigProfHandler (struct sigaction *sa);

/* ---------------------------------------------------------------- */
/*                         MLton libraries                          */
/* ---------------------------------------------------------------- */

/* ------------------------------------------------- */
/*                      IntInf                       */
/* ------------------------------------------------- */

/* All of these routines modify the frontier in gcState.  They assume that 
 * there are bytes bytes free, and allocate an array to store the result
 * at the current frontier position.
 * Immediately after the bytesArg, they take a labelIndex arg.  This is an index
 * into the array used for allocation profiling, and the appropriate element
 * is incremented by the amount that the function moves the frontier.
 */
Pointer IntInf_add (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_andb (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_arshift (Pointer arg, Word shift, size_t bytes);
Pointer IntInf_gcd (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_lshift (Pointer arg, Word shift, size_t bytes);
Pointer IntInf_mul (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_neg (Pointer arg, size_t bytes);
Pointer IntInf_notb (Pointer arg, size_t bytes);
Pointer IntInf_orb (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_quot (Pointer num, Pointer den, size_t bytes);
Pointer IntInf_rem (Pointer num, Pointer den, size_t bytes);
Pointer IntInf_sub (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_toString (Pointer arg, int base, size_t bytes);
Pointer IntInf_xorb (Pointer lhs, Pointer rhs, size_t bytes);

Word IntInf_smallMul (Word lhs, Word rhs, Pointer carry);
Int IntInf_compare (Pointer lhs, Pointer rhs);
Bool IntInf_equal (Pointer lhs, Pointer rhs);

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

void MLton_allocTooLarge (void) __attribute__ ((noreturn));
/* print a bug message and exit (2) */
void MLton_bug (Pointer msg) __attribute__ ((noreturn));

/* ---------------------------------- */
/*           MLton.Platform           */
/* ---------------------------------- */

#define MLton_Platform_Arch_bigendian isBigEndian()

#if (defined (__alpha__))
#define MLton_Platform_Arch_host "alpha"
#elif (defined (__x86_64__))
#define MLton_Platform_Arch_host "amd64"
#elif (defined (__arm__))
#define MLton_Platform_Arch_host "arm"
#elif (defined (__hppa__))
#define MLton_Platform_Arch_host "hppa"
#elif (defined (__ia64__))
#define MLton_Platform_Arch_host "ia64"
#elif (defined (__m68k__))
#define MLton_Platform_Arch_host "m68k"
#elif (defined (__mips__))
#define MLton_Platform_Arch_host "mips"
#elif (defined (__ppc__)) || (defined (__powerpc__))
#define MLton_Platform_Arch_host "powerpc"
#elif (defined (__s390__))
#define MLton_Platform_Arch_host "s390"
#elif (defined (__sparc__))
#define MLton_Platform_Arch_host "sparc"
#elif (defined (__i386__))
#define MLton_Platform_Arch_host "x86"
#else
#error MLton_Platform_Arch_host not defined
#endif

extern Bool MLton_Platform_CygwinUseMmap;

/* ------------------------------------------------- */
/*                     PackReal                      */
/* ------------------------------------------------- */

Real32_t PackReal32_subArr (Array(Word8_t) v, Int offset);
Real32_t PackReal32_subArrRev (Array(Word8_t) v, Int offset);
Real64_t PackReal64_subArr (Array(Word8_t) v, Int offset);
Real64_t PackReal64_subArrRev (Array(Word8_t) v, Int offset);
Real32_t PackReal32_subVec (Vector(Word8_t) v, Int offset);
Real32_t PackReal32_subVecRev (Vector(Word8_t) v, Int offset);
Real64_t PackReal64_subVec (Vector(Word8_t) v, Int offset);
Real64_t PackReal64_subVecRev (Vector(Word8_t) v, Int offset);
void PackReal32_update (Array(Word8_t) a, Int offset, Real32_t r);
void PackReal32_updateRev (Array(Word8_t) a, Int offset, Real32_t r);
void PackReal64_update (Array(Word8_t) a, Int offset, Real64_t r);
void PackReal64_updateRev (Array(Word8_t) a, Int offset, Real64_t r);

/* ------------------------------------------------- */
/*                     PackWord                      */
/* ------------------------------------------------- */

Word16_t PackWord16_subArr (Array(Word8_t) v, Int offset);
Word16_t PackWord16_subArrRev (Array(Word8_t) v, Int offset);
Word32_t PackWord32_subArr (Array(Word8_t) v, Int offset);
Word32_t PackWord32_subArrRev (Array(Word8_t) v, Int offset);
Word64_t PackWord64_subArr (Array(Word8_t) v, Int offset);
Word64_t PackWord64_subArrRev (Array(Word8_t) v, Int offset);
Word16_t PackWord16_subVec (Vector(Word8_t) v, Int offset);
Word16_t PackWord16_subVecRev (Vector(Word8_t) v, Int offset);
Word32_t PackWord32_subVec (Vector(Word8_t) v, Int offset);
Word32_t PackWord32_subVecRev (Vector(Word8_t) v, Int offset);
Word64_t PackWord64_subVec (Vector(Word8_t) v, Int offset);
Word64_t PackWord64_subVecRev (Vector(Word8_t) v, Int offset);
void PackWord16_update (Array(Word8_t) a, Int offset, Word16_t w);
void PackWord16_updateRev (Array(Word8_t) a, Int offset, Word16_t w);
void PackWord32_update (Array(Word8_t) a, Int offset, Word32_t w);
void PackWord32_updateRev (Array(Word8_t) a, Int offset, Word32_t w);
void PackWord64_update (Array(Word8_t) a, Int offset, Word64_t w);
void PackWord64_updateRev (Array(Word8_t) a, Int offset, Word64_t w);
/* Compat */
Word32 Word8Array_subWord32Rev (Pointer v, Int offset);
void Word8Array_updateWord32Rev (Pointer a, Int offset, Word32 w);
Word32 Word8Vector_subWord32Rev (Pointer v, Int offset);

/* ------------------------------------------------- */
/*                       Real                        */
/* ------------------------------------------------- */

Real64 Real64_modf (Real64 x, Real64 *exp);
Real32 Real32_modf (Real32 x, Real32 *exp);
Real64 Real64_frexp (Real64 x, Int *exp);
C_String_t Real64_gdtoa (double d, int mode, int ndig, int *decpt);
C_String_t Real32_gdtoa (float f, int mode, int ndig, int *decpt);
Int Real32_class (Real32 f);
Int Real64_class (Real64 d);
Real32 Real32_strto (Pointer s);
Real64 Real64_strto (Pointer s);
Real64 Real64_nextAfter (Real64 x1, Real64 x2);
Int Real32_signBit (Real32 f);
Int Real64_signBit (Real64 d);
#define ternary(size, name)                                     \
        Real##size Real##size##_mul##name                       \
                (Real##size r1, Real##size r2, Real##size r3);
ternary(32, add)
ternary(64, add)
ternary(32, sub)
ternary(64, sub)
#undef ternary

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

#if (defined (__MSVCRT__))
void MLton_initSockets (void);
#else
static inline void MLton_initSockets (void) {}
#endif

/* ------------------------------------------------- */
/*                  Word{8,16,32,64}                 */
/* ------------------------------------------------- */

#define SaddCheckOverflows(size)                                        \
        Bool WordS##size##_addCheckOverflows (WordS##size x, WordS##size y);
#define UaddCheckOverflows(size)                                        \
        Bool WordU##size##_addCheckOverflows (WordU##size x, WordU##size y);
#define SmulCheckOverflows(size)                                        \
        Bool WordS##size##_mulCheckOverflows (WordS##size x, WordS##size y);
#define negCheckOverflows(size)                                         \
        Bool Word##size##_negCheckOverflows (WordS##size x);
#define SsubCheckOverflows(size)                                        \
        Bool WordS##size##_subCheckOverflows (WordS##size x, WordS##size y);
#define all(size)                                               \
        SaddCheckOverflows (size)                               \
        UaddCheckOverflows (size)                               \
        SmulCheckOverflows (size)                               \
        negCheckOverflows (size)                                \
        SsubCheckOverflows (size)
all (8)
all (16)
all (32)
all (64)
#undef SaddCheckOverflows
#undef UaddCheckOverflows
#undef SmulCheckOverflows
#undef negCheckOverflows
#undef SsubCheckOverflows
#undef all

#endif /* _MLTON_PLATFORM_H_ */
