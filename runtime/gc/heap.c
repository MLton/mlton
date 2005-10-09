/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline bool pointerIsInHeap (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.start <= p 
              and p < s->frontier));
}

static inline bool objptrIsInHeap (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return pointerIsInHeap (s, p);
}

static inline bool pointerIsInOldGen (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.start <= p 
              and p < s->heap.start + s->heap.oldGenSize));
}

static inline bool objptrIsInOldGen (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return pointerIsInOldGen (s, p);
}

static inline bool pointerIsInNursery (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.nursery <= p and p < s->frontier));
}

static inline bool objptrIsInNursery (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return pointerIsInNursery (s, p);
}

static inline bool pointerIsInFromSpace (GC_state s, pointer p) {
  return (pointerIsInOldGen (s, p) or pointerIsInNursery (s, p));
}

static inline bool objptrIsInFromSpace (GC_state s, objptr op) {
  return (objptrIsInOldGen (s, op) or objptrIsInNursery (s, op));
}

#if ASSERT
static bool heapHasBytesFree (GC_state s, size_t oldGen, size_t nursery) {
  size_t total;
  bool res;

  total =
    s->heap.oldGenSize + oldGen 
    + (s->canMinor ? 2 : 1) * (s->limitPlusSlop - s->heap.nursery);
  res = 
    (total <= s->heap.size) 
    and (nursery <= (size_t)(s->limitPlusSlop - s->frontier));
  if (DEBUG_DETAILED)
    fprintf (stderr, "%s = hasBytesFree (%zd, %zd)\n",
             boolToString (res),
             /*uintToCommaString*/(oldGen),
             /*uintToCommaString*/(nursery));
  return res;
}
#endif

void displayHeap (__attribute__ ((unused)) GC_state s,
                  GC_heap heap,
                  FILE *stream) {
  fprintf(stream,
          "\t\tnursery ="FMTPTR"\n"
          "\t\toldGenSize = %zu\n"
          "\t\tstart = "FMTPTR"\n"
          "\t\tsize = %zu\n",
          (uintptr_t)heap->nursery,
          heap->oldGenSize,
          (uintptr_t)heap->start,
          heap->size);
}

/* heapDesiredSize (s, l, c) returns the desired heap size for a heap
 * with l bytes live, given that the current heap size is c.
 */
static size_t heapDesiredSize (GC_state s, size_t live, size_t currentSize) {
  size_t res;
  float ratio;

  ratio = (float)s->sysvals.ram / (float)live;
  if (ratio >= s->ratios.live + s->ratios.grow) {
    /* Cheney copying fits in RAM with desired ratios.live. */
    res = live * s->ratios.live;
    /* If the heap is currently close in size to what we want, leave
     * it alone.  Favor growing over shrinking.
     */
    unless (1.1 * currentSize <= res
            or res <= .5 * currentSize)
      res = currentSize;
  } else if (s->ratios.grow >= s->ratios.copy
             and ratio >= 2 * s->ratios.copy) {
    /* Split RAM in half.  Round down by pageSize so that the total
     * amount of space taken isn't greater than RAM once rounding
     * happens.  This is so resizeHeap2 doesn't get confused and free
     * a semispace in a misguided attempt to avoid paging.
     */
    res = roundDown (s->sysvals.ram / 2, s->sysvals.pageSize);
  } else if (ratio >= s->ratios.copy + s->ratios.grow) {
    /* Cheney copying fits in RAM. */
    res = s->sysvals.ram - s->ratios.grow * live;
    /* If the heap isn't too much smaller than what we want, leave it
     * alone.  On the other hand, if it is bigger we want to leave res
     * as is so that the heap is shrunk, to try to avoid paging.
     */
    if (currentSize <= res 
        and res <= 1.1 * currentSize)
      res = currentSize;
  } else if (ratio >= s->ratios.markCompact) {
    /* Mark compact fits in RAM.  It doesn't matter what the current
     * size is.  If the heap is currently smaller, we are using
     * copying and should switch to mark-compact.  If the heap is
     * currently bigger, we want to shrink back to RAM to avoid
     * paging.
     */
    res = s->sysvals.ram;
  } else { /* Required live ratio. */
    res = live * s->ratios.markCompact;
    /* If the current heap is bigger than res, the shrinking always
     * sounds like a good idea.  However, depending on what pages the
     * VM keeps around, growing could be very expensive, if it
     * involves paging the entire heap.  Hopefully the copy loop in
     * growFromSpace will make the right thing happen.
     */ 
  }
  if (s->control.fixedHeap > 0) {
    if (res > s->control.fixedHeap / 2)
      res = s->control.fixedHeap;
    else
      res = s->control.fixedHeap / 2;
    if (res < live)
      die ("Out of memory with fixed heap size %zd.",
           /*uintToCommaString*/(s->control.fixedHeap));
  } else if (s->control.maxHeap > 0) {
    if (res > s->control.maxHeap)
      res = s->control.maxHeap;
    if (res < live)
      die ("Out of memory with max heap size %zd.",
           /*uintToCommaString*/(s->control.maxHeap));
  }
  if (DEBUG_RESIZING)
    fprintf (stderr, "%zd = heapDesiredSize (%zd, %zd)\n",
             /*uintToCommaString*/(res),
             /*uintToCommaString*/(live),
             /*uintToCommaString*/(currentSize));
  assert (res >= live);
  return res;
}

static inline void heapInit (GC_heap h) {
  h->start = NULL;
  h->size = 0;
  h->oldGenSize = 0;
  h->nursery = NULL;
}

static inline bool heapIsInit (GC_heap h) {
  return 0 == h->size;
}

static void heapRelease (GC_state s, GC_heap h) {
  if (NULL == h->start)
    return;
  if (DEBUG or s->messages)
    fprintf (stderr, "Releasing heap at "FMTPTR" of size %zd.\n",
             (uintptr_t)h->start,
             /*uintToCommaString*/(h->size));
  GC_release (h->start, h->size);
  heapInit (h);
}

static void heapShrink (GC_state s, GC_heap h, size_t keep) {
  assert (keep <= h->size);
  if (0 == keep) {
    heapRelease (s, h);
    return;
  }
  keep = align (keep, s->pageSize);
  if (keep < h->size) {
    if (DEBUG or s->messages)
      fprintf (stderr,
               "Shrinking heap at "FMTPTR" of size %zd to %zd bytes.\n",
               (uintptr_t)h->start,
               /*uintToCommaString*/(h->size),
               /*uintToCommaString*/(keep));
    GC_decommit (h->start + keep, h->size - keep);
    h->size = keep;
  }
}

static void heapSetNursery (GC_state s, 
                            size_t oldGenBytesRequested,
                            size_t nurseryBytesRequested) {
  GC_heap h;
  size_t nurserySize;

  if (DEBUG_DETAILED)
    fprintf (stderr, "setNursery(%zd, %zd)\n",
             /*uintToCommaString*/(oldGenBytesRequested),
             /*uintToCommaString*/(nurseryBytesRequested));
  h = &s->heap;
  assert (isAlignedFrontier (s, h->start + h->oldGenSize + oldGenBytesRequested));
  nurserySize = h->size - h->oldGenSize - oldGenBytesRequested;
  s->limitPlusSlop = h->start + h->size;
  s->limit = s->limitPlusSlop - LIMIT_SLOP;
  assert (isAligned (nurserySize, WORD_SIZE)); // FIXME
  if (/* The mutator marks cards. */
      s->mutatorMarksCards
      /* There is enough space in the nursery. */
      and (nurseryBytesRequested
           <= (size_t)(s->limitPlusSlop
                       - alignFrontier (s, (s->limitPlusSlop 
                                            - nurserySize / 2 + 2))))
      /* The nursery is large enough to be worth it. */
      and (((float)(h->size - s->lastMajorStatistics.bytesLive) 
            / (float)nurserySize) 
           <= s->ratios.nursery)
      and /* There is a reason to use generational GC. */
      (
       /* We must use it for debugging pruposes. */
       FORCE_GENERATIONAL
       /* We just did a mark compact, so it will be advantageous to to
        * use it.
        */
       or (s->lastMajorStatistics.kind == GC_MARK_COMPACT)
       /* The live ratio is low enough to make it worthwhile. */
       or ((float)h->size / (float)s->lastMajorStatistics.bytesLive
           <= (h->size < s->sysvals.ram
               ? s->ratios.copyGenerational
               : s->ratios.markCompactGenerational))
       )) {
    s->canMinor = TRUE;
    nurserySize /= 2;
    while (not (isAligned (nurserySize, WORD_SIZE))) {
      nurserySize -= 2;
    }
    clearCardMap (s);
  } else {
    unless (nurseryBytesRequested
            <= (size_t)(s->limitPlusSlop
                        - alignFrontier (s, s->limitPlusSlop
                                         - nurserySize)))
      die ("Out of memory.  Insufficient space in nursery.");
    s->canMinor = FALSE;
  }
  assert (nurseryBytesRequested
          <= (size_t)(s->limitPlusSlop
                      - alignFrontier (s, s->limitPlusSlop
                                       - nurserySize)));
  s->heap.nursery = alignFrontier (s, s->limitPlusSlop - nurserySize);
  s->frontier = s->heap.nursery;
  assert (nurseryBytesRequested <= (size_t)(s->limitPlusSlop - s->frontier));
  assert (isAlignedFrontier (s, s->heap.nursery));
  assert (heapHasBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
}

/* heapCreate (s, h, desiredSize, minSize) 
 * 
 * allocates a heap of the size necessary to work with desiredSize
 * live data, and ensures that at least minSize is available.  It
 * returns TRUE if it is able to allocate the space, and returns FALSE
 * if it is unable.  If a reasonable size to space is already there,
 * then heapCreate leaves it.
 */
static bool heapCreate (GC_state s, GC_heap h, 
                        size_t desiredSize, 
                        size_t minSize) {
  size_t backoff;

  if (DEBUG_MEM)
    fprintf (stderr, "heapCreate  desired size = %zd  min size = %zd\n",
             /*uintToCommaString*/(desiredSize),
             /*uintToCommaString*/(minSize));
  assert (heapIsInit (h));
  if (desiredSize < minSize)
    desiredSize = minSize;
  desiredSize = align (desiredSize, s->sysvals.pageSize);
  assert (0 == h->size and NULL == h->start);
  backoff = (desiredSize - minSize) / 20;
  if (0 == backoff)
    backoff = 1; /* enough to terminate the loop below */
  backoff = align (backoff, s->sysvals.pageSize);
  /* mmap toggling back and forth between high and low addresses to
   * decrease the chance of virtual memory fragmentation causing an mmap
   * to fail.  This is important for large heaps.
   */
  for (h->size = desiredSize; h->size >= minSize; h->size -= backoff) {
    static bool direction = TRUE;
    unsigned int i;

    assert (isAligned (h->size, s->sysvals.pageSize));
    for (i = 0; i < 32; i++) {
      size_t address;
      
      address = i * 0x08000000ul;
      if (direction)
        address = 0xf8000000ul - address;
      h->start = GC_mmap ((void*)address, h->size);
      if ((void*)-1 == h->start)
        h->start = (void*)NULL;
      unless ((void*)NULL == h->start) {
        direction = not direction;
        if (h->size > s->cumulativeStatistics.maxHeapSizeSeen)
          s->cumulativeStatistics.maxHeapSizeSeen = h->size;
        if (DEBUG or s->messages)
          fprintf (stderr, "Created heap of size %zd at "FMTPTR".\n",
                   /*uintToCommaString*/(h->size),
                   (uintptr_t)h->start);
        assert (h->size >= minSize);
        return TRUE;
      }
    }
    if (s->messages)
      fprintf(stderr, "[Requested %zuM cannot be satisfied, backing off by %zuM (min size = %zuM).\n",
              meg (h->size), meg (backoff), meg (minSize));
  }
  h->size = 0;
  return FALSE;
}
