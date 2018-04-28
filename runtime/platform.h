/* Copyright (C) 2010,2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_PLATFORM_H_
#define _MLTON_PLATFORM_H_

#include "cenv.h"
#include "util.h"
#include "ml-types.h"
#include "c-types.h"

#ifndef MLton_Platform_Arch_host
#error MLton_Platform_Arch_host not defined
#endif

#ifndef MLton_Platform_OS_host
#error MLton_Platform_OS_host not defined
#endif

#ifndef HAS_FEROUND
#error HAS_FEROUND not defined
#endif

#ifndef HAS_MSG_DONTWAIT
#error HAS_MSG_DONTWAIT not defined
#endif

#ifndef HAS_REMAP
#error HAS_REMAP not defined
#endif

#ifndef HAS_SIGALTSTACK
#error HAS_SIGALTSTACK not defined
#else
#ifndef NEEDS_SIGALTSTACK_EXEC
#error NEEDS_SIGALTSTACK_EXEC not defined
#endif
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


#define FE_NOSUPPORT -1

/* With HAS_FEROUND unset, the runtime will provide the implementation.
 * That implementation depends on FE_* having the values we set below.
 * We must therefore make sure to eliminate any existing #defines and
 * then create our own defines, which will also take precedence over
 * any enums we included from system headers.
 */

#if not HAS_FEROUND
#ifdef FE_TONEAREST
#undef FE_TONEAREST
#endif
#ifdef FE_DOWNWARD
#undef FE_DOWNWARD
#endif
#ifdef FE_UPWARD
#undef FE_UPWARD
#endif
#ifdef FE_TOWARDZERO
#undef FE_TOWARDZERO
#endif
#define FE_TONEAREST 0
#define FE_DOWNWARD 1
#define FE_UPWARD 2
#define FE_TOWARDZERO 3
#endif

#ifndef MLTON_CODEGEN_STATIC_INLINE
#define MLTON_CODEGEN_STATIC_INLINE PRIVATE
#endif
#ifndef MLTON_CODEGEN_MATHFN
#define MLTON_CODEGEN_MATHFN(decl)
#endif
#ifndef MLTON_CODEGEN_WORDSQUOTREM
#define MLTON_CODEGEN_WORDSQUOTREM(func) func
#endif
#ifndef MLTON_CODEGEN_WORDSQUOTREM_IMPL
#define MLTON_CODEGEN_WORDSQUOTREM_IMPL(func) func
#endif
#include "basis-ffi.h"

#include "gc.h"

/* ---------------------------------------------------------------- */
/*                        Runtime Init/Exit/Alloc                   */
/* ---------------------------------------------------------------- */

PRIVATE void MLton_init (int argc, char **argv, GC_state s);
PRIVATE __attribute__ ((noreturn)) void MLton_halt (GC_state s, C_Int_t status);
PRIVATE __attribute__ ((noreturn)) void MLton_heapCheckTooLarge (void);

/* ---------------------------------------------------------------- */
/*                        Utility libraries                         */
/* ---------------------------------------------------------------- */

/* ---------------------------------------------------------------- */
/*                        Garbage Collector                         */
/* ---------------------------------------------------------------- */

/* ------------------------------------------------- */
/*                Virtual Memory                     */
/* ------------------------------------------------- */

/* GC_displayMem displays the virtual memory mapping to stdout.
 * It is used to diagnose memory problems.
 */
PRIVATE void GC_displayMem (void);

PRIVATE void *GC_mmapAnon (void *start, size_t length);
PRIVATE void *GC_mmapAnon_safe (void *start, size_t length);
PRIVATE void *GC_mmapAnon_safe_protect (void *start, size_t length, int prot,
                                         size_t dead_low, size_t dead_high);
PRIVATE void *GC_mremap (void *start, size_t oldLength, size_t newLength);
PRIVATE void GC_release (void *base, size_t length);

PRIVATE size_t GC_pageSize (void);
PRIVATE uintmax_t GC_physMem (void);

PRIVATE void GC_setCygwinUseMmap (bool b);

PRIVATE void GC_diskBack_close (void *data);
PRIVATE void GC_diskBack_read (void *data, pointer buf, size_t size);
PRIVATE void *GC_diskBack_write (pointer buf, size_t size);

/* ------------------------------------------------- */
/*                SigProf Handler                    */
/* ------------------------------------------------- */

PRIVATE void GC_setSigProfHandler (struct sigaction *sa);

/* ---------------------------------------------------------------- */
/*                         MLton libraries                          */
/* ---------------------------------------------------------------- */

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

/* ---------------------------------- */
/*           MLton.Platform           */
/* ---------------------------------- */

#define MLton_Platform_Arch_bigendian isBigEndian()

PRIVATE extern Bool MLton_Platform_CygwinUseMmap;

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

#if (defined (__MSVCRT__))
PRIVATE void MLton_initSockets (void);
PRIVATE void MLton_fixSocketErrno (void);
#else
static inline void MLton_initSockets (void) {}
static inline void MLton_fixSocketErrno (void) {}
#endif

#if HAS_MSG_DONTWAIT
#define MLton_recv recv
#define MLton_recvfrom recvfrom
#else
/* Platform has no MSG_DONTWAIT flag for recv(), so these must be
   defined to simulate that flag. */
PRIVATE int MLton_recv(int s, void *buf, int len, int flags);
PRIVATE int MLton_recvfrom(int s, void *buf, int len, int flags, void *from, socklen_t *fromlen);
#endif

#endif /* _MLTON_PLATFORM_H_ */
