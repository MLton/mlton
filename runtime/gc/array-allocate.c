/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

pointer GC_arrayAllocate (GC_state s, 
                          size_t ensureBytesFree, 
                          GC_arrayLength numElements, 
                          GC_header header) {
  uintmax_t arraySizeMax;
  size_t arraySize;
  size_t bytesPerElement;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  pointer frontier;
  pointer last;
  pointer res;

  splitHeader(s, header, NULL, NULL, &bytesNonObjptrs, &numObjptrs);
  if (DEBUG)
    fprintf (stderr, "GC_arrayAllocate (%zu, "FMTARRLEN", "FMTHDR")\n",
             ensureBytesFree, numElements, header);
  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  arraySizeMax = 
    alignMax ((uintmax_t)bytesPerElement * (uintmax_t)numElements + GC_ARRAY_HEADER_SIZE,
              s->alignment);
  if (arraySizeMax >= (uintmax_t)SIZE_MAX)
    die ("Out of memory: cannot allocate array with %s bytes.",
         uintmaxToCommaString(arraySizeMax));
  arraySize = (size_t)arraySizeMax;
  if (arraySize < GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE)
    /* Create space for forwarding pointer. */
    arraySize = GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE;
  if (DEBUG_ARRAY)
    fprintf (stderr, "array with "FMTARRLEN" elts of size %zu and total size %s.  Ensure %s bytes free.\n",
             numElements, bytesPerElement, 
             uintmaxToCommaString(arraySize),
             uintmaxToCommaString(ensureBytesFree));
  if (arraySize >= s->controls.oldGenArraySize) {
    if (not hasHeapBytesFree (s, arraySize, ensureBytesFree)) {
      enter (s);
      performGC (s, arraySize, ensureBytesFree, FALSE, TRUE);
      leave (s);
    }
    frontier = s->heap.start + s->heap.oldGenSize;
    last = frontier + arraySize;
    s->heap.oldGenSize += arraySize;
    s->cumulativeStatistics.bytesAllocated += arraySize;
  } else {
    size_t bytesRequested;

    bytesRequested = arraySize + ensureBytesFree;
    if (not hasHeapBytesFree (s, 0, bytesRequested)) {
      enter (s);
      performGC (s, 0, bytesRequested, FALSE, TRUE);
      leave (s);
    }
    frontier = s->frontier;
    last = frontier + arraySize;
    assert (isFrontierAligned (s, last));
    s->frontier = last;
  }
  *((GC_arrayCounter*)(frontier)) = 0;
  frontier = frontier + GC_ARRAY_COUNTER_SIZE;
  *((GC_arrayLength*)(frontier)) = numElements;
  frontier = frontier + GC_ARRAY_LENGTH_SIZE;
  *((GC_header*)(frontier)) = header;
  frontier = frontier + GC_HEADER_SIZE;
  res = frontier;
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
  GC_profileAllocInc (s, arraySize);
  if (DEBUG_ARRAY) {
    fprintf (stderr, "GC_arrayAllocate done.  res = "FMTPTR"  frontier = "FMTPTR"\n",
             (uintptr_t)res, (uintptr_t)s->frontier);
    displayGCState (s, stderr);
  }
  assert (ensureBytesFree <= (size_t)(s->limitPlusSlop - s->frontier));
  /* Unfortunately, the invariant isn't quite true here, because
   * unless we did the GC, we never set s->currentThread->stack->used
   * to reflect what the mutator did with stackTop.
   */
  return res;
}       
