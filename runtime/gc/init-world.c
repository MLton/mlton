/* Copyright (C) 2011-2012,2014,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

size_t sizeofInitialBytesLive (GC_state s) {
  uint32_t i;
  size_t dataBytes;
  size_t total;

  total = 0;
  for (i = 0; i < s->objectInitsLength; ++i) {
    dataBytes =
      s->objectInits[i].size;
    total += align (dataBytes, s->alignment);
  }
  return total;
}

void initObjects (GC_state s) {
  struct GC_objectInit *inits;
  pointer frontier;
  uint32_t i;
  size_t objectSize;

  assert (isFrontierAligned (s, s->frontier));
  inits = s->objectInits;
  frontier = s->frontier;
  for (i = 0; i < s->objectInitsLength; i++) {
    objectSize = align (inits[i].size, s->alignment);
    assert (objectSize <= (size_t)(s->heap.start + s->heap.size - frontier));
    memcpy (frontier, inits[i].words, inits[i].size);
    s->globals[inits[i].globalIndex] =
      pointerToObjptr(frontier + inits[i].headerOffset, s->heap.start);
    frontier = frontier + objectSize;
    if (DEBUG_DETAILED)
      fprintf (stderr, "allocated object at "FMTPTR"\n",
               (uintptr_t)(s->globals[inits[i].globalIndex]));
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "frontier after global object allocations is "FMTPTR"\n",
             (uintptr_t)frontier);
  GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->cumulativeStatistics.bytesAllocated += (size_t)(frontier - s->frontier);
  assert (isFrontierAligned (s, frontier));
  s->frontier = frontier;
}

void initWorld (GC_state s) {
  uint32_t i;
  pointer start;
  GC_thread thread;

  for (i = 0; i < s->globalsLength; ++i)
    s->globals[i] = BOGUS_OBJPTR;
  s->lastMajorStatistics.bytesLive = sizeofInitialBytesLive (s);
  createHeap (s, &s->heap,
              sizeofHeapDesired (s, s->lastMajorStatistics.bytesLive, 0),
              s->lastMajorStatistics.bytesLive);
  setCardMapAndCrossMap (s);
  start = alignFrontier (s, s->heap.start);
  s->frontier = start;
  s->limitPlusSlop = s->heap.start + s->heap.size;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  initObjects (s);
  assert ((size_t)(s->frontier - start) <= s->lastMajorStatistics.bytesLive);
  s->heap.oldGenSize = (size_t)(s->frontier - s->heap.start);
  setGCStateCurrentHeap (s, 0, 0);
  thread = newThread (s, sizeofStackInitialReserved (s));
  switchToThread (s, pointerToObjptr((pointer)thread - offsetofThread (s), s->heap.start));
}
