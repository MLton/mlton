/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool GC_getAmOriginal (GC_state s);
void GC_setAmOriginal (GC_state s, bool b);
void GC_setMessages (GC_state s, bool b);
void GC_setSummary (GC_state s, bool b);
void GC_setRusageMeasureGC (GC_state s, bool b);
void GC_setHashConsDuringGC (GC_state s, bool b);
struct rusage* GC_getRusageGCAddr (GC_state s);

GC_thread GC_getCurrentThread (GC_state s);
GC_thread GC_getSavedThread (GC_state s);
void GC_setCallFromCHandlerThread (GC_state s, GC_thread thread);
void GC_setSavedThread (GC_state s, GC_thread thread);
void GC_setSignalHandlerThread (GC_state s, GC_thread thread);

sigset_t* GC_getSignalsHandledAddr (GC_state s);
bool GC_getSignalIsPending (GC_state s);
sigset_t* GC_getSignalsPendingAddr (GC_state s);
void GC_setGCSignalHandled (GC_state s, bool b);
void GC_setGCSignalPending (GC_state s, bool b);

