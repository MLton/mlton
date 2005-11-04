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
    numBytes = 
      sizeof(uint32_t) // for the sign
      + strlen (s->intInfInits[i].mlstr);
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
  size_t slen, llen;
  mp_size_t alen;
  uint32_t i, j;
  bool neg, hex;
  GC_intInf bp;
  unsigned char *cp;

  assert (isAlignedFrontier (s, s->frontier));
  frontier = s->frontier;
  for (i= 0; i < s->intInfInitsLength; i++) {
    inits = &s->intInfInits[i];
    str = inits->mlstr;
    assert (inits->globalIndex < s->globalsLength);
    neg = *str == '~';
    if (neg)
      str++;
    slen = strlen (str);
    hex = str[0] == '0' && str[1] == 'x';
    if (hex) {
      str += 2;
      slen -= 2;
      llen = (slen + 7) / 8;
    } else
      llen = (slen + 8) / 9;
    assert (slen > 0);
    bp = (GC_intInf)frontier;
    cp = (unsigned char *)&bp->limbs[llen];

    for (j = 0; j != slen; j++)
      if ('0' <= str[j] && str[j] <= '9')
        cp[j] = str[j] - '0' + 0;
      else if ('a' <= str[j] && str[j] <= 'f')
        cp[j] = str[j] - 'a' + 0xa;
      else {
        assert('A' <= str[j] && str[j] <= 'F');
        cp[j] = str[j] - 'A' + 0xA;
      }
    alen = mpn_set_str ((mp_limb_t*)(bp->limbs), cp, slen, hex ? 0x10 : 10);
    assert ((size_t)alen <= llen);
    if (alen <= 1) {
      uint32_t val, ans;
      
      if (alen == 0)
        val = 0;
      else
        val = bp->limbs[0];
      if (neg) {
        /*
         * We only fit if val in [1, 2^30].
         */
        ans = - val;
        val = val - 1;
      } else
        /* 
         * We only fit if val in [0, 2^30 - 1].
         */
        ans = val;
      if (val < (uint32_t)1<<30) {
        s->globals[inits->globalIndex] = (objptr)(ans<<1 | 1);
        continue;
      }
    }
    s->globals[inits->globalIndex] = pointerToObjptr((pointer)(&bp->isneg), s->heap.start);
    bp->counter = 0;
    bp->length = alen + 1;
    bp->header = buildHeaderFromTypeIndex (WORD32_VECTOR_TYPE_INDEX);
    bp->isneg = neg;
    frontier = alignFrontier (s, (pointer)&bp->limbs[alen]);
  }
  assert (isAlignedFrontier (s, frontier));
  GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->frontier = frontier;
  s->cumulativeStatistics.bytesAllocated += frontier - s->frontier;
}

void initVectors (GC_state s) {
  struct GC_vectorInit *inits;
  pointer frontier;
  uint32_t i;

  assert (isAlignedFrontier (s, s->frontier));
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
    GC_memcpy (inits[i].bytes, frontier, dataBytes);
    frontier += objectSize - GC_ARRAY_HEADER_SIZE;
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "frontier after string allocation is "FMTPTR"\n",
             (uintptr_t)frontier);
  GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->cumulativeStatistics.bytesAllocated += (size_t)(frontier - s->frontier);
  assert (isAlignedFrontier (s, frontier));
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

