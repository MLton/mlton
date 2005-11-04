/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void minorGC (GC_state s);
void majorGC (GC_state s, size_t bytesRequested, bool mayResize);
void growStackCurrent (GC_state s);
void enterGC (GC_state s);
void leaveGC (GC_state s);
void performGC (GC_state s, 
                size_t oldGenBytesRequested,
                size_t nurseryBytesRequested, 
                bool forceMajor,
                bool mayResize);
void ensureInvariantForMutator (GC_state s, bool force);
void ensureHasHeapBytesFree (GC_state s, 
                             size_t oldGenBytesRequested,
                             size_t nurseryBytesRequested);
void GC_gc (GC_state s, size_t bytesRequested, bool force,
            char *file, int line);
