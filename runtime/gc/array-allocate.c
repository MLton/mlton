/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

pointer GC_arrayAllocate (GC_state s,
                          size_t ensureBytesFree,
                          GC_arrayLength numElements,
                          GC_header header) {
  size_t arraySize, arraySizeAligned;
  size_t bytesPerElement;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  pointer frontier;
  pointer last;
  pointer result;

  splitHeader(s, header, NULL, NULL, &bytesNonObjptrs, &numObjptrs);
  if (DEBUG)
    fprintf (stderr, "GC_arrayAllocate (%"PRIuMAX", "FMTARRLEN", "FMTHDR")\n",
             (uintmax_t)ensureBytesFree, numElements, header);
  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  /* Check for overflow when computing arraySize.
   */
  if (bytesPerElement > 0 and numElements > (SIZE_MAX / bytesPerElement)) {
    goto doOverflow;
  }
  arraySize = bytesPerElement * numElements;
  if (arraySize > SIZE_MAX - GC_ARRAY_METADATA_SIZE) {
    goto doOverflow;
  }
  arraySize += GC_ARRAY_METADATA_SIZE;
  arraySizeAligned = align (arraySize, s->alignment);
  if (arraySizeAligned < arraySize) {
    goto doOverflow;
  }
  if (DEBUG_ARRAY)
    fprintf (stderr,
             "Array with "FMTARRLEN" elts of size %"PRIuMAX" and total size %s and total aligned size %s.  "
             "Ensure %s bytes free.\n",
             numElements, (uintmax_t)bytesPerElement,
             uintmaxToCommaString(arraySize),
             uintmaxToCommaString(arraySizeAligned),
             uintmaxToCommaString(ensureBytesFree));
  if (arraySizeAligned >= s->controls.oldGenArraySize) {
    if (not hasHeapBytesFree (s, arraySizeAligned, ensureBytesFree)) {
      enter (s);
      performGC (s, arraySizeAligned, ensureBytesFree, FALSE, TRUE);
      leave (s);
    }
    frontier = s->heap.start + s->heap.oldGenSize;
    s->heap.oldGenSize += arraySizeAligned;
    s->cumulativeStatistics.bytesAllocated += arraySizeAligned;
  } else {
    size_t bytesRequested;
    pointer newFrontier;

    bytesRequested = arraySizeAligned + ensureBytesFree;
    if (not hasHeapBytesFree (s, 0, bytesRequested)) {
      enter (s);
      performGC (s, 0, bytesRequested, FALSE, TRUE);
      leave (s);
    }
    frontier = s->frontier;
    newFrontier = frontier + arraySizeAligned;
    assert (isFrontierAligned (s, newFrontier));
    s->frontier = newFrontier;
  }
  last = frontier + arraySize;
  *((GC_arrayCounter*)(frontier)) = 0;
  frontier = frontier + GC_ARRAY_COUNTER_SIZE;
  *((GC_arrayLength*)(frontier)) = numElements;
  frontier = frontier + GC_ARRAY_LENGTH_SIZE;
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
      /* Array with a mix of pointers and non-pointers. */
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
  GC_profileAllocInc (s, arraySizeAligned);
  if (DEBUG_ARRAY) {
    fprintf (stderr, "GC_arrayAllocate done.  result = "FMTPTR"  frontier = "FMTPTR"\n",
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
  die ("Out of memory.  Unable to allocate array with "FMTARRLEN" elements and elements of size %"PRIuMAX" bytes.",
       numElements, (uintmax_t)bytesPerElement);
}
