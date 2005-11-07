/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool GC_getAmOriginal (GC_state s) {
  return s->amOriginal;
}
void GC_setAmOriginal (GC_state s, bool b) {
  s->amOriginal = b;
}

void GC_setMessages (GC_state s, bool b) {
  s->controls.messages = b;
}

void GC_setSummary (GC_state s, bool b) {
  s->controls.summary = b;
}

void GC_setRusageMeasureGC (GC_state s, bool b) {
  s->controls.rusageMeasureGC = b;
}

void GC_setHashConsDuringGC (GC_state s, bool b) {
  s->hashConsDuringGC = b;
}

struct rusage* GC_getRusageGCAddr (GC_state s) {
  return &(s->cumulativeStatistics.ru_gc);
}

sigset_t* GC_getSignalsHandledAddr (GC_state s) {
  return &(s->signalsInfo.signalsHandled);
}

bool GC_getSignalIsPending (GC_state s) {
  return (s->signalsInfo.signalIsPending);
}

sigset_t* GC_getSignalsPendingAddr (GC_state s) {
  return &(s->signalsInfo.signalsPending);
}

void GC_setGCSignalHandled (GC_state s, bool b) {
  s->signalsInfo.gcSignalHandled = b;
}

void GC_setGCSignalPending (GC_state s, bool b) {
  s->signalsInfo.gcSignalPending = b;
}

void GC_setCallFromCHandlerThread (GC_state s, GC_thread t) {
  objptr op = pointerToObjptr ((pointer)t, s->heap.start);
  s->callFromCHandlerThread = op;
}

GC_thread GC_getCurrentThread (GC_state s) {
  pointer p = objptrToPointer (s->currentThread, s->heap.start);
  return (GC_thread)p;
}

GC_thread GC_getSavedThread (GC_state s) {
  pointer p = objptrToPointer (s->savedThread, s->heap.start);
  s->savedThread = BOGUS_OBJPTR;
  return (GC_thread)p;
}

void GC_setSavedThread (GC_state s, GC_thread t) {
  objptr op = pointerToObjptr ((pointer)t, s->heap.start);
  s->savedThread = op;
}

void GC_setSignalHandlerThread (GC_state s, GC_thread t) {
  objptr op = pointerToObjptr ((pointer)t, s->heap.start);
  s->signalHandlerThread = op;
}
