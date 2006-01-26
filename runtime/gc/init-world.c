/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

size_t sizeofInitialBytesLive (GC_state s) {
  uint32_t i;
  size_t numBytes;
  size_t total;
  
  total = 0;
  for (i = 0; i < s->intInfInitsLength; ++i) {
    /* A slight overestimate. */
    numBytes = 
      sizeof(mp_limb_t) // for the sign
      + (align(strlen (s->intInfInits[i].mlstr), sizeof(mp_limb_t)));
    total += align (GC_ARRAY_HEADER_SIZE 
                    + numBytes, 
                    s->alignment);
  }
  for (i = 0; i < s->vectorInitsLength; ++i) {
    numBytes = 
      s->vectorInits[i].bytesPerElement
      * s->vectorInits[i].numElements;
    total += align (GC_ARRAY_HEADER_SIZE
                    + ((0 == numBytes)
                       ? OBJPTR_SIZE
                       : numBytes),
                    s->alignment);
  }
  return total;
}

void initIntInfs (GC_state s) {
  struct GC_intInfInit *inits;
  pointer frontier;
  char *str;
  size_t slen;
  mp_size_t alen;
  uint32_t i, j;
  bool neg;
  GC_intInf bp;

  assert (isFrontierAligned (s, s->frontier));
  frontier = s->frontier;
  for (i = 0; i < s->intInfInitsLength; i++) {
    inits = &(s->intInfInits[i]);
    str = inits->mlstr;
    assert (inits->globalIndex < s->globalsLength);
    neg = *str == '~';
    if (neg)
      str++;
    slen = strlen (str);
    assert (slen > 0);

    bp = (GC_intInf)frontier;

    for (j = 0; j != slen; j++) {
      assert('0' <= str[j] && str[j] <= '9');
      unsigned char c = str[j] - '0' + 0;
      str[j] = c;
    }
    alen = mpn_set_str ((mp_limb_t*)(bp->limbs), (unsigned char*)str, slen, 10);
    if (alen <= 1) {
      uintmax_t val, ans;
      
      if (alen == 0)
        val = 0;
      else
        val = bp->limbs[0];
      if (neg) {
        /*
         * We only fit if val in [1, 2^(8 * OBJPTR_SIZE - 1)].
         */
        ans = - val;
        val = val - 1;
      } else
        /* 
         * We only fit if val in [0, 2^(8 * OBJPTR_SIZE - 1) - 1].
         */
        ans = val;
      if (val < (uintmax_t)1<<(8 * OBJPTR_SIZE - 1)) {
        s->globals[inits->globalIndex] = (objptr)(ans<<1 | 1);
        continue;
      }
    }
    s->globals[inits->globalIndex] = pointerToObjptr((pointer)(&bp->isneg), s->heap.start);
    bp->counter = 0;
    bp->length = alen + 1;
    bp->header = GC_INTINF_HEADER;
    bp->isneg = neg;
    frontier = alignFrontier (s, (pointer)&bp->limbs[alen]);
  }
  assert (isFrontierAligned (s, frontier));
  GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->frontier = frontier;
  s->cumulativeStatistics.bytesAllocated += frontier - s->frontier;
}

void initVectors (GC_state s) {
  struct GC_vectorInit *inits;
  pointer frontier;
  uint32_t i;

  assert (isFrontierAligned (s, s->frontier));
  inits = s->vectorInits;
  frontier = s->frontier;
  for (i = 0; i < s->vectorInitsLength; i++) {
    size_t bytesPerElement;
    size_t dataBytes;
    size_t objectSize;
    uint32_t typeIndex;

    bytesPerElement = inits[i].bytesPerElement;
    dataBytes = bytesPerElement * inits[i].numElements;
    objectSize = align (GC_ARRAY_HEADER_SIZE
                        + ((0 == dataBytes)
                           ? POINTER_SIZE
                           : dataBytes),
                        s->alignment);
    assert (objectSize <= (size_t)(s->heap.start + s->heap.size - frontier));
    *((GC_arrayCounter*)(frontier)) = 0;
    frontier = frontier + GC_ARRAY_COUNTER_SIZE;
    *((GC_arrayLength*)(frontier)) = inits[i].numElements;
    frontier = frontier + GC_ARRAY_LENGTH_SIZE;
    switch (bytesPerElement) {
    case 1:
      typeIndex = WORD8_VECTOR_TYPE_INDEX;
      break;
    case 2:
      typeIndex = WORD16_VECTOR_TYPE_INDEX;
      break;
    case 4:
      typeIndex = WORD32_VECTOR_TYPE_INDEX;
      break;
    case 8:
      typeIndex = WORD64_VECTOR_TYPE_INDEX;
      break;
    default:
      die ("unknown bytes per element in vectorInit: %zu",
           bytesPerElement);
    }
    *((GC_header*)(frontier)) = buildHeaderFromTypeIndex (typeIndex);
    frontier = frontier + GC_HEADER_SIZE;
    s->globals[inits[i].globalIndex] = pointerToObjptr(frontier, s->heap.start);
    if (DEBUG_DETAILED)
      fprintf (stderr, "allocated vector at "FMTPTR"\n",
               (uintptr_t)(s->globals[inits[i].globalIndex]));
    memcpy (frontier, inits[i].bytes, dataBytes);
    frontier += objectSize - GC_ARRAY_HEADER_SIZE;
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "frontier after string allocation is "FMTPTR"\n",
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
  createCardMapAndCrossMap (s);
  start = alignFrontier (s, s->heap.start);
  s->frontier = start;
  initIntInfs (s);
  initVectors (s);
  assert ((size_t)(s->frontier - start) <= s->lastMajorStatistics.bytesLive);
  s->heap.oldGenSize = s->frontier - s->heap.start;
  setGCStateCurrentHeap (s, 0, 0);
  thread = newThread (s, sizeofStackInitial (s));
  switchToThread (s, pointerToObjptr((pointer)thread, s->heap.start));
}

