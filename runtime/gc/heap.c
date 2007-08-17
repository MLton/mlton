/* Copyright (C) 2005-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void displayHeap (__attribute__ ((unused)) GC_state s,
                  GC_heap heap,
                  FILE *stream) {
  fprintf(stream,
          "\t\tnursery = "FMTPTR"\n"
          "\t\toldGenSize = %"PRIuMAX"\n"
          "\t\tsize = %"PRIuMAX"\n"
          "\t\tstart = "FMTPTR"\n",
          (uintptr_t)heap->nursery,
          (uintmax_t)heap->oldGenSize,
          (uintmax_t)heap->size,
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
  if (ratio >= s->controls.ratios.live + s->controls.ratios.grow) {
    /* Cheney copying fits in RAM with desired ratios.live. */
    res = live * s->controls.ratios.live;
    /* If the heap is currently close in size to what we want, leave
     * it alone.  Favor growing over shrinking.
     */
    unless (1.1 * currentSize <= res
            or res <= .5 * currentSize)
      res = currentSize;
  } else if (s->controls.ratios.grow >= s->controls.ratios.copy
             and ratio >= 2 * s->controls.ratios.copy) {
    /* Split RAM in half.  Round down by pageSize so that the total
     * amount of space taken isn't greater than RAM once rounding
     * happens.  This is so resizeHeapSecondary doesn't get confused
     * and free a semispace in a misguided attempt to avoid paging.
     */
    res = alignDown (s->sysvals.ram / 2, s->sysvals.pageSize);
  } else if (ratio >= s->controls.ratios.copy + s->controls.ratios.grow) {
    /* Cheney copying fits in RAM. */
    res = s->sysvals.ram - s->controls.ratios.grow * live;
    /* If the heap isn't too much smaller than what we want, leave it
     * alone.  On the other hand, if it is bigger we want to leave res
     * as is so that the heap is shrunk, to try to avoid paging.
     */
    if (currentSize <= res
        and res <= 1.1 * currentSize)
      res = currentSize;
  } else if (ratio >= s->controls.ratios.markCompact) {
    /* Mark compact fits in RAM.  It doesn't matter what the current
     * size is.  If the heap is currently smaller, we are using
     * copying and should switch to mark-compact.  If the heap is
     * currently bigger, we want to shrink back to RAM to avoid
     * paging.
     */
    res = s->sysvals.ram;
  } else { /* Required live ratio. */
    res = live * s->controls.ratios.markCompact;
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
      die ("Out of memory with fixed heap size %s.",
           uintmaxToCommaString(s->controls.fixedHeap));
  } else if (s->controls.maxHeap > 0) {
    if (res > s->controls.maxHeap)
      res = s->controls.maxHeap;
    if (res < live)
      die ("Out of memory with max heap size %s.",
           uintmaxToCommaString(s->controls.maxHeap));
  }
  if (DEBUG_RESIZING)
    fprintf (stderr, "%s = sizeofHeapDesired (%s, %s)\n",
             uintmaxToCommaString(res),
             uintmaxToCommaString(live),
             uintmaxToCommaString(currentSize));
  assert (res >= live);
  return res;
}

void releaseHeap (GC_state s, GC_heap h) {
  if (NULL == h->start)
    return;
  if (DEBUG or s->controls.messages)
    fprintf (stderr,
             "[GC: Releasing heap at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(h->start),
             uintmaxToCommaString(h->size));
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
               "[GC: Shrinking heap at "FMTPTR" of size %s bytes to size %s bytes.]\n",
               (uintptr_t)(h->start),
               uintmaxToCommaString(h->size),
               uintmaxToCommaString(keep));
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
    fprintf (stderr, "createHeap  desired size = %s  min size = %s\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(minSize));
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
   * Note that the loop always trys a NULL address last.
   */
  for (h->size = desiredSize; h->size >= minSize; h->size -= backoff) {
    const unsigned int countLog2 = 5;
    const unsigned int count = 0x1 << countLog2;
    const size_t step = (size_t)0x1 << ((POINTER_SIZE * CHAR_BIT) - countLog2);

    static bool direction = TRUE;
    unsigned int i;

    assert (isAligned (h->size, s->sysvals.pageSize));
    for (i = 1; i <= count; i++) {
      size_t address;

      address = (size_t)i * step;
      if (direction)
        address = (size_t)0x0 - address;
      h->start = GC_mmapAnon ((pointer)address, h->size);
      if ((void*)-1 == h->start)
        h->start = (void*)NULL;
      unless ((void*)NULL == h->start) {
        direction = not direction;
        if (h->size > s->cumulativeStatistics.maxHeapSizeSeen)
          s->cumulativeStatistics.maxHeapSizeSeen = h->size;
        if (DEBUG or s->controls.messages)
          fprintf (stderr,
                   "[GC: Created heap at "FMTPTR" of size %s bytes.]\n",
                   (uintptr_t)(h->start),
                   uintmaxToCommaString(h->size));
        assert (h->size >= minSize);
        return TRUE;
      }
    }
    if (s->controls.messages) {
      fprintf (stderr,
               "[GC: Creating heap of size %s bytes cannot be satisfied,]\n",
               uintmaxToCommaString (h->size));
      fprintf (stderr,
               "[GC:\tbacking off by %s bytes with minimum size of %s bytes.]\n",
               uintmaxToCommaString (backoff),
               uintmaxToCommaString (minSize));
    }
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

  assert (desiredSize >= s->heap.size);
  if (DEBUG_RESIZING or s->controls.messages) {
    fprintf (stderr,
             "[GC: Growing heap at "FMTPTR" of size %s bytes,]\n",
             (uintptr_t)s->heap.start,
             uintmaxToCommaString(s->heap.size));
    fprintf (stderr,
             "[GC:\tto desired size of %s bytes and minimum size of %s bytes.]\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(minSize));
  }
  curHeapp = &s->heap;
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
    newHeap.oldGenSize = size;
    *curHeapp = newHeap;
  } else {
    /* Write the heap to disk and try again. */
    void *data;

    if (DEBUG or s->controls.messages) {
      fprintf (stderr,
               "[GC: Writing heap at "FMTPTR" of size %s bytes to disk.]\n",
               (uintptr_t)orig,
               uintmaxToCommaString(size));
    }
    data = GC_diskBack_write (orig, size);
    releaseHeap (s, curHeapp);
    if (createHeap (s, curHeapp, desiredSize, minSize)) {
      if (DEBUG or s->controls.messages) {
        fprintf (stderr,
                 "[GC: Reading heap at "FMTPTR" of size %s bytes from disk.]\n",
                 (uintptr_t)orig,
                 uintmaxToCommaString(size));
      }
      GC_diskBack_read (data, curHeapp->start, size);
      GC_diskBack_close (data);
    } else {
      GC_diskBack_close (data);
      if (s->controls.messages)
        GC_displayMem ();
      die ("Out of memory.  Unable to allocate %s bytes.\n",
           uintmaxToCommaString(minSize));
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
    fprintf (stderr, "resizeHeap  minSize = %s  size = %s\n",
             uintmaxToCommaString(minSize),
             uintmaxToCommaString(s->heap.size));
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
