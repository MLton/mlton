/* Copyright (C) 2012,2016,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                    Cheney Copying Collection                     */
/* ---------------------------------------------------------------- */

void updateWeaksForCheneyCopy (GC_state s) {
  pointer p;
  GC_weak w;

  for (w = s->weaks; w != NULL; w = w->link) {
    assert (BOGUS_OBJPTR != w->objptr);

    if (DEBUG_WEAK)
      fprintf (stderr, "updateWeaksForCheneyCopy  w = "FMTPTR"  ", (uintptr_t)w);
    p = objptrToPointer (w->objptr, s->heap.start);
    if (hasFwdPtr(p)) {
      if (DEBUG_WEAK)
        fprintf (stderr, "forwarded from "FMTOBJPTR" to "FMTOBJPTR"\n",
                 w->objptr, getFwdPtr(p));
      w->objptr = getFwdPtr(p);
    } else {
      if (DEBUG_WEAK)
        fprintf (stderr, "cleared\n");
      *(getHeaderp((pointer)w - offsetofWeak (s))) = GC_WEAK_GONE_HEADER;
      w->objptr = BOGUS_OBJPTR;
    }
  }
  s->weaks = NULL;
}

void swapHeapsForCheneyCopy (GC_state s) {
  struct GC_heap tempHeap;

  tempHeap = s->secondaryHeap;
  s->secondaryHeap = s->heap;
  s->heap = tempHeap;
  setCardMapAndCrossMap (s);
}

void majorCheneyCopyGC (GC_state s) {
  size_t bytesCopied;
  struct GC_forwardState forwardState;
  struct GC_foreachObjptrClosure forwardObjptrIfInHeapClosure;
  struct rusage ru_start;
  pointer toStart;

  assert (s->secondaryHeap.size >= s->heap.oldGenSize);
  if (detailedGCTime (s))
    startTiming (&ru_start);
  s->cumulativeStatistics.numCopyingGCs++;
  if (DEBUG or s->controls.messages) {
    fprintf (stderr, 
             "[GC: Starting major Cheney-copy;]\n");
    fprintf (stderr,
             "[GC:\tfrom heap at "FMTPTR" of size %s bytes,]\n",
             (uintptr_t)(s->heap.start), 
             uintmaxToCommaString(s->heap.size));
    fprintf (stderr, 
             "[GC:\tto heap at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(s->secondaryHeap.start), 
             uintmaxToCommaString(s->secondaryHeap.size));
  }
  assert (s->secondaryHeap.start != (pointer)NULL);
  /* The next assert ensures there is enough space for the copy to
   * succeed.  It does not assert 
   *   (s->secondaryHeap.size >= s->heap.size) 
   * because that is too strong.
   */
  assert (s->secondaryHeap.size >= s->heap.oldGenSize);
  toStart = alignFrontier (s, s->secondaryHeap.start);
  forwardState.amInMinorGC = FALSE;
  forwardState.toStart = s->secondaryHeap.start;
  forwardState.toLimit = s->secondaryHeap.start + s->secondaryHeap.size;
  forwardState.back = toStart;
  forwardObjptrIfInHeapClosure.fun = forwardObjptrIfInHeapFun;
  forwardObjptrIfInHeapClosure.env = &forwardState;
  foreachGlobalObjptr (s, &forwardObjptrIfInHeapClosure);
  foreachObjptrInRange (s, toStart, &forwardState.back,
                        &forwardObjptrIfInHeapClosure, TRUE);
  updateWeaksForCheneyCopy (s);
  s->secondaryHeap.oldGenSize = (size_t)(forwardState.back - s->secondaryHeap.start);
  bytesCopied = s->secondaryHeap.oldGenSize;
  s->cumulativeStatistics.bytesCopied += bytesCopied;
  swapHeapsForCheneyCopy (s);
  s->lastMajorStatistics.kind = GC_COPYING;
  if (detailedGCTime (s))
    stopTiming (&ru_start, &s->cumulativeStatistics.ru_gcCopying);
  if (DEBUG or s->controls.messages)
    fprintf (stderr, 
             "[GC: Finished major Cheney-copy; copied %s bytes.]\n",
             uintmaxToCommaString(bytesCopied));
}

/* ---------------------------------------------------------------- */
/*                 Minor Cheney Copying Collection                  */
/* ---------------------------------------------------------------- */

void minorCheneyCopyGC (GC_state s) {
  size_t bytesAllocated;
  size_t bytesCopied;
  struct GC_forwardState forwardState;
  struct GC_foreachObjptrClosure forwardObjptrIfInNurseryClosure;
  struct rusage ru_start;

  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "minorGC  nursery = "FMTPTR"  frontier = "FMTPTR"\n",
             (uintptr_t)s->heap.nursery, (uintptr_t)s->frontier);
  assert (invariantForGC (s));
  bytesAllocated = (size_t)(s->frontier - s->heap.nursery);
  if (bytesAllocated == 0)
    return;
  s->cumulativeStatistics.bytesAllocated += bytesAllocated;
  if (not s->canMinor) {
    s->heap.oldGenSize += bytesAllocated;
  } else {
    if (detailedGCTime (s))
      startTiming (&ru_start);
    s->cumulativeStatistics.numMinorGCs++;
    if (DEBUG_GENERATIONAL or s->controls.messages) {
      fprintf (stderr, 
               "[GC: Starting minor Cheney-copy;]\n");
      fprintf (stderr,
               "[GC:\tfrom nursery at "FMTPTR" of size %s bytes.]\n",
               (uintptr_t)(s->heap.nursery),
               uintmaxToCommaString(bytesAllocated));
    }
    assert (invariantForGC (s));
    forwardState.amInMinorGC = TRUE;
    forwardState.toStart = s->heap.start + s->heap.oldGenSize;
    forwardState.toLimit = forwardState.toStart + bytesAllocated;
    forwardState.back = forwardState.toStart;
    forwardObjptrIfInNurseryClosure.fun = forwardObjptrIfInNurseryFun;
    forwardObjptrIfInNurseryClosure.env = &forwardState;
    assert (isFrontierAligned (s, forwardState.toStart));
    /* Forward all globals.  Would like to avoid doing this once all
     * the globals have been assigned.
     */
    foreachGlobalObjptr (s, &forwardObjptrIfInNurseryClosure);
    forwardInterGenerationalObjptrs (s, &forwardState);
    foreachObjptrInRange (s, forwardState.toStart, &forwardState.back,
                          &forwardObjptrIfInNurseryClosure, TRUE);
    updateWeaksForCheneyCopy (s);
    bytesCopied = (size_t)(forwardState.back - forwardState.toStart);
    s->cumulativeStatistics.bytesCopiedMinor += bytesCopied;
    s->heap.oldGenSize += bytesCopied;
    s->lastMajorStatistics.numMinorGCs++;
    if (detailedGCTime (s))
      stopTiming (&ru_start, &s->cumulativeStatistics.ru_gcMinor);
    if (DEBUG_GENERATIONAL or s->controls.messages)
      fprintf (stderr, 
               "[GC: Finished minor Cheney-copy; copied %s bytes.]\n",
               uintmaxToCommaString(bytesCopied));
  }
}
