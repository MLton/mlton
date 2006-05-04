/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
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
#elif (defined (__hpux__))
#include "platform/hpux.h"
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

#ifndef SPAWN_MODE
#define SPAWN_MODE 0
#endif

#include "ml-types.h"
#include "c-types.h"
#include "basis-ffi.h"

/* ---------------------------------------------------------------- */
/*                        Runtime Init/Exit                         */
/* ---------------------------------------------------------------- */

void MLton_init (int argc, char **argv, GC_state s);
void MLton_exit (GC_state s, C_Int_t status) __attribute__ ((noreturn));

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
/*                       MLton                       */
/* ------------------------------------------------- */

void MLton_allocTooLarge (void) __attribute__ ((noreturn));

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
/*                     PackWord                      */
/* ------------------------------------------------- */

/* Compat */
Word32_t Word8Array_subWord32Rev (Array(Word8_t) a, C_Ptrdiff_t offset);
void Word8Array_updateWord32Rev (Array(Word8_t) a, C_Ptrdiff_t offset, Word32_t w);
Word32_t Word8Vector_subWord32Rev (Vector(Word8_t) v, C_Ptrdiff_t offset);

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
