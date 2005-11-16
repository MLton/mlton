/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void copyForThreadInternal (pointer dst, pointer src);
static inline void threadInternalObjptr (GC_state s, objptr *opp);
static inline void clearIfWeakAndUnmarkedForMarkCompact (GC_state s, pointer p);
static void updateForwardPointersForMarkCompact (GC_state s);
static void updateBackwardPointersAndSlideForMarkCompact (GC_state s);
static void majorMarkCompactGC (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
