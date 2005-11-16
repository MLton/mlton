/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void minorGC (GC_state s);
static void majorGC (GC_state s, size_t bytesRequested, bool mayResize);
static inline void growStackCurrent (GC_state s);
static inline void enterGC (GC_state s);
static inline void leaveGC (GC_state s);
static void performGC (GC_state s, 
                       size_t oldGenBytesRequested,
                       size_t nurseryBytesRequested, 
                       bool forceMajor,
                       bool mayResize);
static inline void ensureInvariantForMutator (GC_state s, bool force);
static inline void ensureHasHeapBytesFree (GC_state s, 
                                           size_t oldGenBytesRequested,
                                           size_t nurseryBytesRequested);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_collect (GC_state s, size_t bytesRequested, bool force,
                 char *file, int line);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
