/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void minorGC (GC_state s);
void majorGC (GC_state s, size_t bytesRequested, bool mayResize);
void enterGC (GC_state s);
void leaveGC (GC_state s);
bool needGCTime (GC_state s);
void doGC (GC_state s, 
           size_t oldGenBytesRequested,
           size_t nurseryBytesRequested, 
           bool forceMajor,
           bool mayResize);
void ensureMutatorInvariant (GC_state s, bool force);
void ensureFree (GC_state s, size_t bytesRequested);
void switchToThread (GC_state s, objptr op);
void GC_startHandler (GC_state s);
void GC_finishHandler (GC_state s);
void maybeSwitchToHandler (GC_state s);
void GC_switchToThread (GC_state s, GC_thread t, size_t ensureBytesFree);
void GC_gc (GC_state s, size_t bytesRequested, bool force,
            char *file, int line);
