/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void rusageZero (struct rusage *ru);
static inline void rusagePlusMax (struct rusage *ru1,
                                  struct rusage *ru2,
                                  struct rusage *ru);
static inline void rusageMinusMax (struct rusage *ru1,
                                   struct rusage *ru2,
                                   struct rusage *ru);
static inline uintmax_t rusageTime (struct rusage *ru);
static inline uintmax_t getCurrentTime (void);
static inline void startTiming (struct rusage *ru_start);
static uintmax_t stopTiming (struct rusage *ru_start, struct rusage *ru_gc);

static inline void timevalZero (struct timeval *tv);
static void timevalPlusMax (struct timeval *tv1,
                            struct timeval *tv2,
                            struct timeval *tv);
static void timevalMinusMax (struct timeval *tv1,
                             struct timeval *tv2,
                             struct timeval *tv);
static inline uintmax_t timevalTime (struct timeval *tv);
static inline void startWallTiming (struct timeval *tv_start);
static uintmax_t stopWallTiming (struct timeval *tv_start, struct timeval *tv_acc);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
