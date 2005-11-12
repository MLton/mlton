/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void copyForThreadInternal (pointer dst, pointer src);
void threadInternalObjptr (GC_state s, objptr *opp);
void clearIfWeakAndUnmarkedForMarkCompact (GC_state s, pointer p);
void updateForwardPointersForMarkCompact (GC_state s);
void updateBackwardPointersAndSlideForMarkCompact (GC_state s);
void majorMarkCompactGC (GC_state s);
