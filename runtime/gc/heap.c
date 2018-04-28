/* Copyright (C) 2009-2012 Matthew Fluet.
 * Copyright (C) 2005-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void displayHeap (__attribute__ ((unused)) GC_state s,
                  GC_heap heap,
                  FILE *stream) {
  fprintf(stream,
          "\t\tnursery = "FMTPTR"\n"
          "\t\toldGenSize = %"PRIuMAX"\n"
          "\t\tsize = %"PRIuMAX"\n"
          "\t\tstart = "FMTPTR"\n"
          "\t\twithMapsSize = %"PRIuMAX"\n",
          (uintptr_t)heap->nursery,
          (uintmax_t)heap->oldGenSize,
          (uintmax_t)heap->size,
          (uintptr_t)heap->start,
          (uintmax_t)heap->withMapsSize);
}


void initHeap (__attribute__ ((unused)) GC_state s,
               GC_heap h) {
  h->nursery = NULL;
  h->oldGenSize = 0;
  h->size = 0;
  h->start = NULL;
  h->withMapsSize = 0;
}

/* sizeofHeapDesired (s, l, cs)
 *
 * returns the desired heap size for a heap with l bytes live,
 * given that the current heap size is cs.
 */
size_t sizeofHeapDesired (GC_state s, size_t liveSize, size_t currentSize) {
  size_t liveMapsSize, liveWithMapsSize;
  size_t currentMapsSize, currentWithMapsSize;
  size_t resSize, resWithMapsSize;
  size_t syslimSize, syslimWithMapsSize;
  LOCAL_USED_FOR_ASSERT size_t syslimMapsSize;
  double ratio;

  syslimWithMapsSize = alignDown (SIZE_MAX, s->sysvals.pageSize);
  syslimSize = invertSizeofCardMapAndCrossMap (s, syslimWithMapsSize);
  syslimMapsSize = sizeofCardMapAndCrossMap (s, syslimSize);
  assert (syslimSize + syslimMapsSize <= syslimWithMapsSize);

  liveSize = align (liveSize, s->sysvals.pageSize);
  if (syslimSize < liveSize)
    die ("Out of memory with system-limit heap size %s.\n",
         uintmaxToCommaString(syslimSize));
  liveMapsSize = sizeofCardMapAndCrossMap (s, liveSize);
  liveWithMapsSize = liveSize + liveMapsSize;

  assert (isAligned (currentSize, s->sysvals.pageSize));
  currentMapsSize = sizeofCardMapAndCrossMap (s, currentSize);
  currentWithMapsSize = currentSize + currentMapsSize;

  ratio = (double)s->sysvals.ram / (double)liveWithMapsSize;

  if (ratio >= s->controls.ratios.live + s->controls.ratios.grow) {
    /* Cheney copying fits in RAM with desired ratios.live. */
    resWithMapsSize = (size_t)(liveWithMapsSize * s->controls.ratios.live);
    /* If the heap is currently close in size to what we want, leave
     * it alone.  Favor growing over shrinking.
     */
    if (0.5 * currentWithMapsSize <= resWithMapsSize
        and resWithMapsSize <= 1.1 * currentWithMapsSize) {
      resWithMapsSize = currentWithMapsSize;
    } else {
      resWithMapsSize = align (resWithMapsSize, s->sysvals.pageSize);
    }
  } else if (s->controls.ratios.grow >= s->controls.ratios.copy
             and ratio >= 2.0 * s->controls.ratios.copy) {
    /* Split RAM in half.  Round down by pageSize so that the total
     * amount of space taken isn't greater than RAM once rounding
     * happens.  This is so resizeHeapSecondary doesn't get confused
     * and free a semispace in a misguided attempt to avoid paging.
     */
    resWithMapsSize = alignDown (s->sysvals.ram / 2, s->sysvals.pageSize);
  } else if (ratio >= s->controls.ratios.copy + s->controls.ratios.grow) {
    /* Cheney copying fits in RAM. */
    resWithMapsSize = s->sysvals.ram - (size_t)(s->controls.ratios.grow * liveWithMapsSize);
    /* If the heap isn't too much smaller than what we want, leave it
     * alone.  On the other hand, if it is bigger we want to leave
     * resWithMapsSize as is so that the heap is shrunk, to try to
     * avoid paging.
     */
    if (1.0 * currentWithMapsSize <= resWithMapsSize
        and resWithMapsSize <= 1.1 * currentWithMapsSize) {
      resWithMapsSize = currentWithMapsSize;
    } else {
      resWithMapsSize = align (resWithMapsSize, s->sysvals.pageSize);
    }
  } else if (ratio >= s->controls.ratios.markCompact) {
    /* Mark compact fits in RAM.  It doesn't matter what the current
     * size is.  If the heap is currently smaller, we are using
     * copying and should switch to mark-compact.  If the heap is
     * currently bigger, we want to shrink back to RAM to avoid
     * paging.
     */
    resWithMapsSize = s->sysvals.ram;
  } else { /* Required live ratio. */
    double resWithMapsSizeD = liveWithMapsSize * (double)(s->controls.ratios.markCompact);
    if (resWithMapsSizeD > (double)syslimWithMapsSize) {
      resWithMapsSize = syslimWithMapsSize;
    } else {
      resWithMapsSize = align ((size_t)resWithMapsSizeD, s->sysvals.pageSize);
    }
    /* If the current heap is bigger than resWithMapsSize, then
     * shrinking always sounds like a good idea.  However, depending
     * on what pages the VM keeps around, growing could be very
     * expensive, if it involves paging the entire heap.  Hopefully
     * the copy loop in growHeap will make the right thing happen.
     */
  }
  if (s->controls.fixedHeap > 0) {
    if (resWithMapsSize > s->controls.fixedHeap / 2)
      resWithMapsSize = s->controls.fixedHeap;
    else
      resWithMapsSize = s->controls.fixedHeap / 2;
    if (resWithMapsSize < liveWithMapsSize)
      die ("Out of memory with fixed heap size %s.",
           uintmaxToCommaString(s->controls.fixedHeap));
  } else if (s->controls.maxHeap > 0) {
    if (resWithMapsSize > s->controls.maxHeap)
      resWithMapsSize = s->controls.maxHeap;
    if (resWithMapsSize < liveWithMapsSize)
      die ("Out of memory with max heap size %s.",
           uintmaxToCommaString(s->controls.maxHeap));
  }
  resSize = invertSizeofCardMapAndCrossMap (s, resWithMapsSize);
  assert (isAligned (resSize, s->sysvals.pageSize));
  if (DEBUG_RESIZING)
    fprintf (stderr, "%s = sizeofHeapDesired (%s, %s)\n",
             uintmaxToCommaString(resSize),
             uintmaxToCommaString(liveSize),
             uintmaxToCommaString(currentSize));
  assert (resSize >= liveSize);
  return resSize;
}

void releaseHeap (GC_state s, GC_heap h) {
  if (NULL == h->start)
    return;
  if (DEBUG or s->controls.messages)
    fprintf (stderr,
             "[GC: Releasing heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map).]\n",
             (uintptr_t)(h->start),
             uintmaxToCommaString(h->size),
             uintmaxToCommaString(h->withMapsSize - h->size));
  GC_release (h->start, h->withMapsSize);
  initHeap (s, h);
}

/* shrinkHeap (s, h, keepSize)
 */
void shrinkHeap (GC_state s, GC_heap h, size_t keepSize) {
  assert (keepSize <= h->size);
  if (0 == keepSize) {
    releaseHeap (s, h);
    return;
  }
  keepSize = align (keepSize, s->sysvals.pageSize);
  if (keepSize < h->size) {
    size_t keepWithMapsSize;
    keepWithMapsSize = keepSize + sizeofCardMapAndCrossMap (s, keepSize);
    if (DEBUG or s->controls.messages) {
      fprintf (stderr,
               "[GC: Shrinking heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map)]\n",
               (uintptr_t)(h->start),
               uintmaxToCommaString(h->size),
               uintmaxToCommaString(h->withMapsSize - h->size));
      fprintf (stderr,
               "[GC:\tto size %s bytes (+ %s bytes card/cross map).]\n",
               uintmaxToCommaString(keepSize),
               uintmaxToCommaString(keepWithMapsSize - keepSize));
    }
    assert (isAligned (keepWithMapsSize, s->sysvals.pageSize));
    assert (keepWithMapsSize <= h->withMapsSize);
    GC_release (h->start + keepWithMapsSize, h->withMapsSize - keepWithMapsSize);
    h->size = keepSize;
    h->withMapsSize = keepWithMapsSize;
  }
}

/* createHeap (s, h, desiredSize, minSize)
 *
 * allocates a heap of the size necessary to work with desiredSize
 * live data, and ensures that at least minSize is available.  It
 * returns TRUE if it is able to allocate the space, and returns FALSE
 * if it is unable.
 */
bool createHeap (GC_state s, GC_heap h,
                 size_t desiredSize,
                 size_t minSize) {
  size_t newSize;
  size_t newWithMapsSize;

  if (DEBUG_MEM)
    fprintf (stderr, "createHeap  desired size = %s  min size = %s\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(minSize));
  if (desiredSize < minSize)
    desiredSize = minSize;
  minSize = align (minSize, s->sysvals.pageSize);
  desiredSize = align (desiredSize, s->sysvals.pageSize);
  assert (isHeapInit (h) and NULL == h->start);
  /* Biased binary search (between minSize and desiredSize) for a
   * successful mmap.
   * Toggle back and forth between high and low addresses to decrease
   * the chance of virtual memory fragmentation; important for large
   * heaps.
   * Always try a NULL address last.
   */
  size_t factor = 16;
  const size_t maxFactor = s->sysvals.pageSize;
  size_t lowSize = minSize;
  size_t highSize = desiredSize;
  newSize = highSize;
  unsigned int loopCount = 0;
  while (lowSize <= highSize) {
    pointer newStart;

    newWithMapsSize = newSize + sizeofCardMapAndCrossMap (s, newSize);

    assert (isAligned (newWithMapsSize, s->sysvals.pageSize));

    const unsigned int addressCountLog2 = 5;
    const unsigned int addressCount = 0x1 << addressCountLog2;
    const size_t addressStep = (size_t)0x1 << (ADDRESS_BITS - addressCountLog2);
#if ADDRESS_BITS == POINTER_BITS
    const size_t addressHigh = 0;
#else
    const size_t addressHigh = (size_t)0x1 << ADDRESS_BITS;
#endif
    static bool addressScanDir = TRUE;
    for (unsigned int i = 1; i <= addressCount; i++) {
      size_t address = (size_t)i * addressStep;
      if (addressScanDir)
        address = addressHigh - address;
      /* Always use 0 in the last step. */
      if (i == addressCount)
        address = 0;

      newStart = GC_mmapAnon ((pointer)address, newWithMapsSize);
      unless ((void*)-1 == newStart) {
        addressScanDir = not addressScanDir;
        h->start = newStart;
        h->size = newSize;
        h->withMapsSize = newWithMapsSize;
        if (h->size > s->cumulativeStatistics.maxHeapSize)
          s->cumulativeStatistics.maxHeapSize = h->size;
        assert (minSize <= h->size and h->size <= desiredSize);
        if (DEBUG or s->controls.messages)
          fprintf (stderr,
                   "[GC: Created heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map).]\n",
                   (uintptr_t)(h->start),
                   uintmaxToCommaString(h->size),
                   uintmaxToCommaString(h->withMapsSize - h->size));
        return TRUE;
      }
    }
    size_t prevSize = newSize;
    size_t prevWithMapsSize = newWithMapsSize;
    highSize = newSize - s->sysvals.pageSize;
    newSize = align((factor-1) * (highSize / factor) + (lowSize / factor), s->sysvals.pageSize);
    if (s->controls.messages) {
      fprintf (stderr,
               "[GC: Creating heap of size %s bytes (+ %s bytes card/cross map) cannot be satisfied,]\n",
               uintmaxToCommaString (prevSize),
               uintmaxToCommaString (prevWithMapsSize - prevSize));
      fprintf (stderr,
               "[GC:\tbacking off by %s bytes with minimum size of %s bytes.]\n",
               uintmaxToCommaString (prevSize - newSize),
               uintmaxToCommaString (minSize));
    }
    if (factor < maxFactor
        and ++loopCount % 64 == 0) {
      factor += factor;
    }
  }
  return FALSE;
}

/* createHeapSecondary (s, desiredSize)
 */
bool createHeapSecondary (GC_state s, size_t desiredSize) {
  size_t desiredWithMapsSize;
  size_t minSize, minWithMapsSize;
  desiredWithMapsSize = desiredSize + sizeofCardMapAndCrossMap (s, desiredSize);
  if ((s->controls.fixedHeap > 0
       and s->heap.withMapsSize + desiredWithMapsSize > s->controls.fixedHeap)
      or (s->controls.maxHeap > 0
          and s->heap.withMapsSize + desiredWithMapsSize > s->controls.maxHeap))
    return FALSE;
  minSize = align (s->heap.oldGenSize, s->sysvals.pageSize);
  minWithMapsSize = minSize + sizeofCardMapAndCrossMap (s, minSize);
  if (minWithMapsSize > SIZE_MAX - s->heap.withMapsSize)
    return FALSE;
  return createHeap (s, &s->secondaryHeap, desiredSize, s->heap.oldGenSize);
}

/* remapHeap (s, h, desiredSize, minSize)
 */
#if not HAS_REMAP
bool remapHeap (__attribute__ ((unused)) GC_state s,
                __attribute__ ((unused)) GC_heap h,
                __attribute__ ((unused)) size_t desiredSize,
                __attribute__ ((unused)) size_t minSize) {
  return FALSE;
}
#else
bool remapHeap (GC_state s, GC_heap h,
                size_t desiredSize,
                size_t minSize) {

  size_t newSize;
  size_t newWithMapsSize;
  int result;

  if (DEBUG_MEM)
    fprintf (stderr, "remapHeap  desired size = %s  min size = %s\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(minSize));
  assert (minSize <= desiredSize);
  assert (desiredSize >= h->size);
  minSize = align (minSize, s->sysvals.pageSize);
  desiredSize = align (desiredSize, s->sysvals.pageSize);

  /* Biased binary search (between minSize and desiredSize) for a
   * successful mremap.
   */
  size_t factor = 16;
  size_t lowSize = minSize;
  size_t highSize = desiredSize;
  newSize = highSize;
  result = FALSE;
  while (lowSize <= highSize) {
    pointer newStart;

    newWithMapsSize = newSize + sizeofCardMapAndCrossMap (s, newSize);

    assert (isAligned (newWithMapsSize, s->sysvals.pageSize));

    newStart = GC_mremap (h->start, h->withMapsSize, newWithMapsSize);
    if ((void*)-1 != newStart) {
      pointer origStart = h->start;
      size_t origSize = h->size;
      size_t origWithMapsSize = h->withMapsSize;
      h->start = newStart;
      h->size = newSize;
      h->withMapsSize = newWithMapsSize;
      if (h->size > s->cumulativeStatistics.maxHeapSize)
        s->cumulativeStatistics.maxHeapSize = h->size;
      assert (minSize <= h->size and h->size <= desiredSize);
      if (DEBUG or s->controls.messages) {
        fprintf (stderr,
                 "[GC: Remapped heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map)]\n",
                 (uintptr_t)origStart,
                 uintmaxToCommaString(origSize),
                 uintmaxToCommaString(origWithMapsSize - origSize));
        fprintf (stderr,
                 "[GC:\tto heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map).]\n",
                 (uintptr_t)(h->start),
                 uintmaxToCommaString(h->size),
                 uintmaxToCommaString(h->withMapsSize - h->size));
      }
      lowSize = newSize + s->sysvals.pageSize;
      newSize = align((factor-1) * (highSize / factor) + (lowSize / factor), s->sysvals.pageSize);
      result = TRUE;
    } else {
      size_t prevSize = newSize;
      size_t prevWithMapsSize = newWithMapsSize;
      highSize = newSize - s->sysvals.pageSize;
      newSize = align((factor-1) * (highSize / factor) + (lowSize / factor), s->sysvals.pageSize);
      if (s->controls.messages) {
        fprintf (stderr,
                 "[GC: Remapping heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map)]\n",
                 (uintptr_t)(h->start),
                 uintmaxToCommaString (h->size),
                 uintmaxToCommaString (h->withMapsSize - h->size));
        fprintf (stderr,
                 "[GC:\tto heap of size %s bytes (+ %s bytes card/cross map) cannot be satisfied,]\n",
                 uintmaxToCommaString (prevSize),
                 uintmaxToCommaString (prevWithMapsSize - prevSize));
        if (result) {
          fprintf (stderr,
                   "[GC:\tbacking off by %s bytes.]\n",
                   uintmaxToCommaString (prevSize - newSize));
        } else {
          fprintf (stderr,
                   "[GC:\tbacking off by %s bytes with minimum size of %s bytes.]\n",
                   uintmaxToCommaString (prevSize - newSize),
                   uintmaxToCommaString (minSize));
        }
      }
    }
  }
  return result;
}
#endif

enum {
  COPY_CHUNK_SIZE = 0x2000000, /* 32M */
};

/* growHeap (s, desiredSize, minSize)
 */
void growHeap (GC_state s, size_t desiredSize, size_t minSize) {
  GC_heap curHeapp;
  struct GC_heap newHeap;
  GC_heap newHeapp;

  bool useCurrent;
  pointer origStart;
  size_t liveSize;

  assert (isAligned (desiredSize, s->sysvals.pageSize));
  assert (isAligned (minSize, s->sysvals.pageSize));
  assert (desiredSize >= s->heap.size);
  if (DEBUG_RESIZING or s->controls.messages) {
    fprintf (stderr,
             "[GC: Growing heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map),]\n",
             (uintptr_t)s->heap.start,
             uintmaxToCommaString(s->heap.size),
             uintmaxToCommaString(s->heap.withMapsSize - s->heap.size));
    fprintf (stderr,
             "[GC:\tto desired size of %s bytes (+ %s bytes card/cross map)]\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(sizeofCardMapAndCrossMap (s, desiredSize)));
    fprintf (stderr,
             "[GC:\tand minimum size of %s bytes (+ %s bytes card/cross map).]\n",
             uintmaxToCommaString(minSize),
             uintmaxToCommaString(sizeofCardMapAndCrossMap (s, minSize)));
  }
  if (minSize <= s->heap.size) {
    useCurrent = TRUE;
    /* Demand proper growth from remapHeap and/or createHeap. */
    minSize = s->heap.size + s->sysvals.pageSize;
  } else {
    useCurrent = FALSE;
  }
  curHeapp = &s->heap;
  newHeapp = &newHeap;
  origStart = curHeapp->start;
  liveSize = curHeapp->oldGenSize;
  assert (liveSize <= curHeapp->size);
  if (remapHeap (s, curHeapp, desiredSize, minSize)) {
    goto done;
  }
  if (!useCurrent)
    shrinkHeap (s, curHeapp, liveSize);
  initHeap (s, newHeapp);
  /* Allocate a space of the desired size. */
  if (minSize + sizeofCardMapAndCrossMap (s, minSize) <= SIZE_MAX - curHeapp->withMapsSize
      and createHeap (s, newHeapp, desiredSize, minSize)) {
    pointer from;
    pointer to;
    size_t remaining;

    from = curHeapp->start + liveSize;
    to = newHeapp->start + liveSize;
    remaining = liveSize;
    shrinkHeap (s, curHeapp, remaining);
copy:
    assert (remaining == (size_t)(from - curHeapp->start)
            and from >= curHeapp->start
            and to >= newHeapp->start);
    if (remaining < COPY_CHUNK_SIZE) {
      GC_memcpy (curHeapp->start, newHeapp->start, remaining);
      releaseHeap (s, curHeapp);
    } else {
      remaining -= COPY_CHUNK_SIZE;
      from -= COPY_CHUNK_SIZE;
      to -= COPY_CHUNK_SIZE;
      GC_memcpy (from, to, COPY_CHUNK_SIZE);
      shrinkHeap (s, curHeapp, remaining);
      goto copy;
    }
    newHeapp->oldGenSize = liveSize;
    *curHeapp = *newHeapp;
  } else if (useCurrent) {
    if (DEBUG_RESIZING or s->controls.messages) {
      fprintf (stderr,
               "[GC: Using heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map).]\n",
               (uintptr_t)s->heap.start,
               uintmaxToCommaString(s->heap.size),
               uintmaxToCommaString(s->heap.withMapsSize - s->heap.size));
    }
  } else if (s->controls.mayPageHeap) {
    /* Page the heap to disk and try again. */
    void *data;

    if (DEBUG or s->controls.messages) {
      fprintf (stderr,
               "[GC: Writing heap at "FMTPTR" of size %s bytes to disk.]\n",
               (uintptr_t)curHeapp->start,
               uintmaxToCommaString(liveSize));
    }
    data = GC_diskBack_write (curHeapp->start, liveSize);
    releaseHeap (s, curHeapp);
    if (createHeap (s, curHeapp, desiredSize, minSize)) {
      if (DEBUG or s->controls.messages) {
        fprintf (stderr,
                 "[GC: Reading heap to "FMTPTR" of size %s bytes from disk.]\n",
                 (uintptr_t)(curHeapp->start),
                 uintmaxToCommaString(liveSize));
      }
      GC_diskBack_read (data, curHeapp->start, liveSize);
      GC_diskBack_close (data);
      curHeapp->oldGenSize = liveSize;
    } else {
      GC_diskBack_close (data);
      goto oom;
    }
  } else {
    goto oom;
  }
done:
  unless (origStart == s->heap.start) {
    translateHeap (s, origStart, s->heap.start, s->heap.oldGenSize);
  }
  return;
oom:
  if (s->controls.messages)
    GC_displayMem ();
  die ("Out of memory.  Unable to allocate heap with %s bytes.\n",
       uintmaxToCommaString(minSize));
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
  assert (isAligned (desiredSize, s->sysvals.pageSize));
  assert (minSize <= desiredSize);
  minSize = align (minSize, s->sysvals.pageSize);
  if (desiredSize <= s->heap.size) {
    shrinkHeap (s, &s->heap, desiredSize);
  } else {
    releaseHeap (s, &s->secondaryHeap);
    growHeap (s, desiredSize, minSize);
  }
  assert (s->heap.size >= minSize);
}

/* resizeHeapSecondary (s)
 */
void resizeHeapSecondary (GC_state s) {
  size_t primarySize, primaryWithMapsSize;
  size_t secondarySize;

  primarySize = s->heap.size;
  primaryWithMapsSize = s->heap.withMapsSize;
  secondarySize = s->secondaryHeap.size;
  if (DEBUG_RESIZING)
    fprintf (stderr, "secondaryHeapResize\n");
  if (0 == secondarySize)
    return;
  if (2 * primaryWithMapsSize > s->sysvals.ram)
    /* Holding on to secondaryHeap might cause paging.  So don't. */
    releaseHeap (s, &s->secondaryHeap);
  else if (secondarySize < primarySize) {
    unless (remapHeap (s, &s->secondaryHeap, primarySize, primarySize))
      releaseHeap (s, &s->secondaryHeap);
  } else if (secondarySize > primarySize)
    shrinkHeap (s, &s->secondaryHeap, primarySize);
  assert (0 == s->secondaryHeap.size
          or s->heap.size == s->secondaryHeap.size);
}
