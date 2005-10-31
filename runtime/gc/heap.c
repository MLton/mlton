/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void displayHeap (__attribute__ ((unused)) GC_state s,
                  GC_heap heap,
                  FILE *stream) {
  fprintf(stream,
          "\t\tnursery ="FMTPTR"\n"
          "\t\toldGenSize = %zu\n"
          "\t\tsize = %zu\n"
          "\t\tstart = "FMTPTR"\n",
          (uintptr_t)heap->nursery,
          heap->oldGenSize,
          heap->size,
          (uintptr_t)heap->start);
}


void initHeap (__attribute__ ((unused)) GC_state s,
               GC_heap h) {
  h->nursery = NULL;
  h->oldGenSize = 0;
  h->size = 0;
  h->start = NULL;
}

/* sizeofHeapDesired (s, l, cs) 
 *
 * returns the desired heap size for a heap with l bytes live, given
 * that the current heap size is cs.
 */
size_t sizeofHeapDesired (GC_state s, size_t live, size_t currentSize) {
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
    res = alignDown (s->sysvals.ram / 2, s->sysvals.pageSize);
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
    /* If the current heap is bigger than res, then shrinking always
     * sounds like a good idea.  However, depending on what pages the
     * VM keeps around, growing could be very expensive, if it
     * involves paging the entire heap.  Hopefully the copy loop in
     * growHeap will make the right thing happen.
     */ 
  }
  if (s->controls.fixedHeap > 0) {
    if (res > s->controls.fixedHeap / 2)
      res = s->controls.fixedHeap;
    else
      res = s->controls.fixedHeap / 2;
    if (res < live)
      die ("Out of memory with fixed heap size %zu.",
           /*uintToCommaString*/(s->controls.fixedHeap));
  } else if (s->controls.maxHeap > 0) {
    if (res > s->controls.maxHeap)
      res = s->controls.maxHeap;
    if (res < live)
      die ("Out of memory with max heap size %zu.",
           /*uintToCommaString*/(s->controls.maxHeap));
  }
  if (DEBUG_RESIZING)
    fprintf (stderr, "%zu = sizeofHeapDesired (%zu, %zu)\n",
             /*uintToCommaString*/(res),
             /*uintToCommaString*/(live),
             /*uintToCommaString*/(currentSize));
  assert (res >= live);
  return res;
}

void releaseHeap (GC_state s, GC_heap h) {
  if (NULL == h->start)
    return;
  if (DEBUG or s->controls.messages)
    fprintf (stderr, "Releasing heap at "FMTPTR" of size %zu.\n",
             (uintptr_t)h->start,
             /*uintToCommaString*/(h->size));
  GC_release (h->start, h->size);
  initHeap (s, h);
}

void shrinkHeap (GC_state s, GC_heap h, size_t keep) {
  assert (keep <= h->size);
  if (0 == keep) {
    releaseHeap (s, h);
    return;
  }
  keep = align (keep, s->sysvals.pageSize);
  if (keep < h->size) {
    if (DEBUG or s->controls.messages)
      fprintf (stderr,
               "Shrinking heap at "FMTPTR" of size %zu to %zu bytes.\n",
               (uintptr_t)h->start,
               /*uintToCommaString*/(h->size),
               /*uintToCommaString*/(keep));
    GC_decommit (h->start + keep, h->size - keep);
    h->size = keep;
  }
}

/* createHeap (s, h, desiredSize, minSize) 
 * 
 * allocates a heap of the size necessary to work with desiredSize
 * live data, and ensures that at least minSize is available.  It
 * returns TRUE if it is able to allocate the space, and returns FALSE
 * if it is unable.  If a reasonable size to space is already there,
 * then heapCreate leaves it.
 */
bool createHeap (GC_state s, GC_heap h, 
                 size_t desiredSize, 
                 size_t minSize) {
  size_t backoff;

  if (DEBUG_MEM)
    fprintf (stderr, "createHeap  desired size = %zu  min size = %zu\n",
             /*uintToCommaString*/(desiredSize),
             /*uintToCommaString*/(minSize));
  assert (isHeapInit (h));
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
    const size_t highAddress = ((size_t)0xf8) << ((POINTER_SIZE - 1) * CHAR_BIT);
    const size_t step = (size_t)0x08000000;
    const size_t count = highAddress / step;

    static bool direction = TRUE;
    unsigned int i;

    assert (isAligned (h->size, s->sysvals.pageSize));
    for (i = 0; i <= count; i++) {
      size_t address;
      
      address = i * step;
      if (direction)
        address = highAddress - address;
      h->start = GC_mmapAnon ((pointer)address, h->size);
      if ((void*)-1 == h->start)
        h->start = (void*)NULL;
      unless ((void*)NULL == h->start) {
        direction = not direction;
        if (h->size > s->cumulativeStatistics.maxHeapSizeSeen)
          s->cumulativeStatistics.maxHeapSizeSeen = h->size;
        if (DEBUG or s->controls.messages)
          fprintf (stderr, "Created heap of size %zu at "FMTPTR".\n",
                   /*uintToCommaString*/(h->size),
                   (uintptr_t)h->start);
        assert (h->size >= minSize);
        return TRUE;
      }
    }
    if (s->controls.messages)
      fprintf(stderr, 
              "[Requested %zuM cannot be satisfied, "
              "backing off by %zuM (min size = %zuM).\n",
              meg (h->size), meg (backoff), meg (minSize));
  }
  h->size = 0;
  return FALSE;
}

/* createHeapSecondary (s, desiredSize)
 */
bool createHeapSecondary (GC_state s, size_t desiredSize) {
  if ((s->controls.fixedHeap > 0 
       and s->heap.size + desiredSize > s->controls.fixedHeap)
      or (s->controls.maxHeap > 0 
          and s->heap.size + desiredSize > s->controls.maxHeap))
    return FALSE;
  return createHeap (s, &s->secondaryHeap, desiredSize, s->heap.oldGenSize);
}

/* remapHeap (s, h, desiredSize, minSize)
 */
bool remapHeap (GC_state s, GC_heap h, 
                size_t desiredSize, 
                size_t minSize) {
  size_t backoff;
  size_t size;

#if not HAS_REMAP
  return FALSE;
#endif
  assert (minSize <= desiredSize);
  assert (desiredSize >= h->size);
  desiredSize = align (desiredSize, s->sysvals.pageSize);
  backoff = (desiredSize - minSize) / 20;
  if (0 == backoff)
    backoff = 1; /* enough to terminate the loop below */
  backoff = align (backoff, s->sysvals.pageSize);
  for (size = desiredSize; size >= minSize; size -= backoff) {
    pointer new;

    new = GC_mremap (h->start, h->size, size);
    unless ((void*)-1 == new) {
      h->start = new;
      h->size = size;
      if (h->size > s->cumulativeStatistics.maxHeapSizeSeen)
        s->cumulativeStatistics.maxHeapSizeSeen = h->size;
      assert (minSize <= h->size and h->size <= desiredSize);
      return TRUE;
    }
  }
  return FALSE;
}

enum {
  COPY_CHUNK_SIZE = 0x2000000, /* 32M */
};

/* growHeap (s, desiredSize, minSize)
 */
void growHeap (GC_state s, size_t desiredSize, size_t minSize) {
  GC_heap curHeapp;
  struct GC_heap newHeap;

  pointer orig;
  size_t size;

  curHeapp = &s->heap;
  assert (desiredSize >= s->heap.size);
  if (DEBUG_RESIZING)
    fprintf (stderr, "Growing heap at "FMTPTR" of size %zu to %zu bytes.\n",
             (uintptr_t)s->heap.start,
             /*uintToCommaString*/(s->heap.size),
             /*uintToCommaString*/(desiredSize));
  orig = curHeapp->start;
  size = curHeapp->oldGenSize;
  assert (size <= s->heap.size);
  if (remapHeap (s, curHeapp, desiredSize, minSize))
    goto done;
  shrinkHeap (s, curHeapp, size);
  initHeap (s, &newHeap);
  /* Allocate a space of the desired size. */
  if (createHeap (s, &newHeap, desiredSize, minSize)) {
    pointer from;
    pointer to;
    size_t remaining;

    from = curHeapp->start + size;
    to = newHeap.start + size;
    remaining = size;
copy:                   
    assert (remaining == (size_t)(from - curHeapp->start)
            and from >= curHeapp->start
            and to >= newHeap.start);
    if (remaining < COPY_CHUNK_SIZE) {
      GC_memcpy (orig, newHeap.start, remaining);
    } else {
      remaining -= COPY_CHUNK_SIZE;
      from -= COPY_CHUNK_SIZE;
      to -= COPY_CHUNK_SIZE;
      GC_memcpy (from, to, COPY_CHUNK_SIZE);
      shrinkHeap (s, curHeapp, remaining);
      goto copy;
    }
    releaseHeap (s, curHeapp);
    *curHeapp = newHeap;
  } else {
    /* Write the heap to a file and try again. */
    int fd;
    char template[80];
    char *tmpDefault;
    char *tmpDir;
    char *tmpVar;
    
#if (defined (__MSVCRT__))
    tmpVar = "TEMP";
    tmpDefault = "C:/WINNT/TEMP";
#else
    tmpVar = "TMPDIR";
    tmpDefault = "/tmp";
#endif
    tmpDir = getenv (tmpVar);
    strcpy (template, (NULL == tmpDir) ? tmpDefault : tmpDir);
    strcat (template, "/FromSpaceXXXXXX");
    fd = mkstemp_safe (template);
    close_safe (fd);
    if (s->controls.messages)
      fprintf (stderr, "Paging heap from "FMTPTR" to %s.\n", 
               (uintptr_t)orig, template);
    fd = open_safe (template, O_WRONLY, 0);
    write_safe (fd, orig, size);
    close_safe (fd);
    releaseHeap (s, curHeapp);
    if (createHeap (s, curHeapp, desiredSize, minSize)) {
      fd = open_safe (template, O_RDONLY, 0);
      read_safe (fd, curHeapp->start, size);
      close_safe (fd);
      unlink_safe (template);
    } else {
      unlink_safe (template);
      if (s->controls.messages)
        showMem ();
      die ("Out of memory.  Unable to allocate %zu bytes.\n",
           /*uintToCommaString*/(minSize));
    }
  }
done:
  unless (orig == s->heap.start) {
    translateHeap (s, orig, s->heap.start, s->heap.oldGenSize);
    setCardMapAbsolute (s);
  }
}

/* resizeHeap (s, minSize)
 */
void resizeHeap (GC_state s, size_t minSize) {
  size_t desiredSize;

  if (DEBUG_RESIZING)
    fprintf (stderr, "resizeHeap  minSize = %zu  size = %zu\n",
             /*ullongToCommaString*/(minSize), 
             /*uintToCommaString*/(s->heap.size));
  desiredSize = sizeofHeapDesired (s, minSize, s->heap.size);
  assert (minSize <= desiredSize);
  if (desiredSize <= s->heap.size)
    shrinkHeap (s, &s->heap, desiredSize);
  else {
    releaseHeap (s, &s->secondaryHeap);
    growHeap (s, desiredSize, minSize);
  }
  resizeCardMapAndCrossMap (s);
  assert (s->heap.size >= minSize);
}

/* resizeHeapSecondary (s)
 */
void resizeHeapSecondary (GC_state s) {
  size_t primarySize;
  size_t secondarySize;

  primarySize = s->heap.size;
  secondarySize = s->secondaryHeap.size;
  if (DEBUG_RESIZING)
    fprintf (stderr, "secondaryHeapResize\n");
  if (0 == secondarySize)
    return;
  if (2 * primarySize > s->sysvals.ram)
    /* Holding on to heap2 might cause paging.  So don't. */
    releaseHeap (s, &s->secondaryHeap);
  else if (secondarySize < primarySize) {
    unless (remapHeap (s, &s->secondaryHeap, primarySize, primarySize))
      releaseHeap (s, &s->secondaryHeap);
  } else if (secondarySize > primarySize)
    shrinkHeap (s, &s->secondaryHeap, primarySize);
  assert (0 == s->secondaryHeap.size 
          or s->heap.size == s->secondaryHeap.size);
}


void setHeapNursery (GC_state s, 
                     size_t oldGenBytesRequested,
                     size_t nurseryBytesRequested) {
  GC_heap h;
  size_t nurserySize;

  if (DEBUG_DETAILED)
    fprintf (stderr, "setHeapNursery(%zu, %zu)\n",
             /*uintToCommaString*/(oldGenBytesRequested),
             /*uintToCommaString*/(nurseryBytesRequested));
  h = &s->heap;
  assert (isAlignedFrontier (s, h->start + h->oldGenSize + oldGenBytesRequested));
  nurserySize = h->size - h->oldGenSize - oldGenBytesRequested;
  s->limitPlusSlop = h->start + h->size;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  assert (isAligned (nurserySize, POINTER_SIZE));
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
    while (not (isAligned (nurserySize, POINTER_SIZE))) {
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
  assert (hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
}
