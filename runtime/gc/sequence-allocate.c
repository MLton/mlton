/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

pointer GC_sequenceAllocate (GC_state s,
                              size_t ensureBytesFree,
                              GC_sequenceLength numElements,
                              GC_header header) {
  size_t sequenceSize, sequenceSizeAligned;
  size_t bytesPerElement;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  pointer frontier;
  pointer last;
  pointer result;

  splitHeader(s, header, NULL, NULL, &bytesNonObjptrs, &numObjptrs);
  if (DEBUG)
    fprintf (stderr, "GC_sequenceAllocate (%"PRIuMAX", "FMTSEQLEN", "FMTHDR")\n",
             (uintmax_t)ensureBytesFree, numElements, header);
  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  /* Check for overflow when computing sequenceSize.
   */
  if (bytesPerElement > 0 and numElements > (SIZE_MAX / bytesPerElement)) {
    goto doOverflow;
  }
  sequenceSize = bytesPerElement * numElements;
  if (sequenceSize > SIZE_MAX - GC_SEQUENCE_METADATA_SIZE) {
    goto doOverflow;
  }
  sequenceSize += GC_SEQUENCE_METADATA_SIZE;
  sequenceSizeAligned = align (sequenceSize, s->alignment);
  if (sequenceSizeAligned < sequenceSize) {
    goto doOverflow;
  }
  if (DEBUG_SEQUENCE)
    fprintf (stderr,
             "Sequence with "FMTSEQLEN" elts of size %"PRIuMAX" and total size %s and total aligned size %s.  "
             "Ensure %s bytes free.\n",
             numElements, (uintmax_t)bytesPerElement,
             uintmaxToCommaString(sequenceSize),
             uintmaxToCommaString(sequenceSizeAligned),
             uintmaxToCommaString(ensureBytesFree));
  if (sequenceSizeAligned >= s->controls.oldGenSequenceSize) {
    if (not hasHeapBytesFree (s, sequenceSizeAligned, ensureBytesFree)) {
      enter (s);
      performGC (s, sequenceSizeAligned, ensureBytesFree, FALSE, TRUE);
      leave (s);
    }
    frontier = s->heap.start + s->heap.oldGenSize;
    s->heap.oldGenSize += sequenceSizeAligned;
    s->cumulativeStatistics.bytesAllocated += sequenceSizeAligned;
  } else {
    size_t bytesRequested;
    pointer newFrontier;

    bytesRequested = sequenceSizeAligned + ensureBytesFree;
    if (not hasHeapBytesFree (s, 0, bytesRequested)) {
      enter (s);
      performGC (s, 0, bytesRequested, FALSE, TRUE);
      leave (s);
    }
    frontier = s->frontier;
    newFrontier = frontier + sequenceSizeAligned;
    assert (isFrontierAligned (s, newFrontier));
    s->frontier = newFrontier;
  }
  last = frontier + sequenceSize;
  *((GC_sequenceCounter*)(frontier)) = 0;
  frontier = frontier + GC_SEQUENCE_COUNTER_SIZE;
  *((GC_sequenceLength*)(frontier)) = numElements;
  frontier = frontier + GC_SEQUENCE_LENGTH_SIZE;
  *((GC_header*)(frontier)) = header;
  frontier = frontier + GC_HEADER_SIZE;
  result = frontier;
  assert (isAligned ((size_t)result, s->alignment));
  /* Initialize all pointers with BOGUS_OBJPTR. */
  if (1 <= numObjptrs and 0 < numElements) {
    pointer p;

    if (0 == bytesNonObjptrs)
      for (p = frontier; p < last; p += OBJPTR_SIZE)
        *((objptr*)p) = BOGUS_OBJPTR;
    else {
      /* Sequence with a mix of pointers and non-pointers. */
      size_t bytesObjptrs;

      bytesObjptrs = numObjptrs * OBJPTR_SIZE;

      for (p = frontier; p < last; ) {
        pointer next;

        p += bytesNonObjptrs;
        next = p + bytesObjptrs;
        assert (next <= last);
        for ( ; p < next; p += OBJPTR_SIZE)
          *((objptr*)p) = BOGUS_OBJPTR;
      }
    }
  }
  GC_profileAllocInc (s, sequenceSizeAligned);
  if (DEBUG_SEQUENCE) {
    fprintf (stderr, "GC_sequenceAllocate done.  result = "FMTPTR"  frontier = "FMTPTR"\n",
             (uintptr_t)result, (uintptr_t)s->frontier);
    displayGCState (s, stderr);
  }
  assert (ensureBytesFree <= (size_t)(s->limitPlusSlop - s->frontier));
  /* Unfortunately, the invariant isn't quite true here, because
   * unless we did the GC, we never set s->currentThread->stack->used
   * to reflect what the mutator did with stackTop.
   */
  return result;

doOverflow:
  die ("Out of memory.  Unable to allocate sequence with "FMTSEQLEN" elements and elements of size %"PRIuMAX" bytes.",
       numElements, (uintmax_t)bytesPerElement);
}
