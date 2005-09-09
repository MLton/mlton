/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

typedef void (*GC_pointerFun) (GC_state s, objptr *pp);

static inline void maybeCall (GC_pointerFun f, GC_state s, objptr *pp) {
  if (isObjptr (*pp))
    f (s, pp);
}

/* foreachGlobalObjptr (s, f)
 * 
 * Apply f to each global object pointer into the heap. 
 */
static inline void foreachGlobalObjptr (GC_state s, GC_pointerFun f) {
  for (unsigned int i = 0; i < s->globalsSize; ++i) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "foreachGlobal %u\n", i);
    maybeCall (f, s, &s->globals [i]);
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "foreachGlobal threads\n");
  maybeCall (f, s, &s->callFromCHandler);
  maybeCall (f, s, &s->currentThread);
  maybeCall (f, s, &s->savedThread);
  maybeCall (f, s, &s->signalHandler);
}


/* foreachObjptrInObject (s, p, skipWeaks, f) 
 * 
 * Applies f to each object pointer in the object pointed to by p.
 * Returns pointer to the end of object, i.e. just past object.
 *
 * If skipWeaks, then the object pointer in weak objects is skipped.
 */
static inline pointer foreachObjptrInObject (GC_state s, 
                                             pointer p,
                                             bool skipWeaks,
                                             GC_pointerFun f) {
  bool hasIdentity;
  GC_header header;
  uint16_t numNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  header = getHeader (p);
  SPLIT_HEADER();
  if (DEBUG_DETAILED)
    fprintf (stderr, 
             "foreachObjptrInObject ("FMTPTR")"
             "  header = "FMTHDR
             "  tag = %s"
             "  numNonObjptrs = %d"
             "  numObjptrs = %d\n", 
             (uintptr_t)p, header, tagToString (tag), 
             numNonObjptrs, numObjptrs);
  if (NORMAL_TAG == tag) {
    p += numNonObjptrsToBytes(numNonObjptrs, NORMAL_TAG);
    pointer max = p + (numObjptrs * OBJPTR_SIZE);
    /* Apply f to all internal pointers. */
    for ( ; p < max; p += OBJPTR_SIZE) {
      if (DEBUG_DETAILED)
        fprintf (stderr, 
                 "  p = "FMTPTR"  *p = "FMTOBJPTR"\n",
                 (uintptr_t)p, *(objptr*)p);
      maybeCall (f, s, (objptr*)p);
    }
  } else if (WEAK_TAG == tag) {
    p += numNonObjptrsToBytes(numNonObjptrs, NORMAL_TAG);
    if (not skipWeaks and 1 == numObjptrs) {
      maybeCall (f, s, (objptr*)p);
      p += OBJPTR_SIZE;
    }
  } else if (ARRAY_TAG == tag) {
    size_t bytesPerElement;
    size_t dataBytes;
    pointer max;
    GC_arrayLength numElements;
    
    numElements = arrayNumElements (p);
    bytesPerElement = 
      numNonObjptrsToBytes(numNonObjptrs, ARRAY_TAG) 
      + (numObjptrs * OBJPTR_SIZE);
    dataBytes = numElements * bytesPerElement;
    /* Must check 0 == dataBytes before 0 == numPointers to correctly
     * handle arrays when both are true.
     */
    if (0 == dataBytes)
      /* Empty arrays have space for forwarding pointer. */
      dataBytes = OBJPTR_SIZE;
    else if (0 == numObjptrs)
      /* No pointers to process. */
      ;
    else {
      max = p + dataBytes;
      if (0 == numNonObjptrs)
        /* Array with only pointers. */
        for ( ; p < max; p += OBJPTR_SIZE)
          maybeCall (f, s, (objptr*)p);
      else {
        /* Array with a mix of pointers and non-pointers. */
        size_t nonObjptrBytes;
        size_t objptrBytes;
        
        nonObjptrBytes = numNonObjptrsToBytes(numNonObjptrs, ARRAY_TAG);
        objptrBytes = numObjptrs * OBJPTR_SIZE;

        /* For each array element. */
        while (p < max) {
          pointer max2;
          
          /* Skip the non-pointers. */
          p += nonObjptrBytes;
          max2 = p + objptrBytes;
          /* For each internal pointer. */
          for ( ; p < max2; p += OBJPTR_SIZE) 
            maybeCall (f, s, (objptr*)p);
        }
      }
      assert (p == max);
      p -= dataBytes;
    }
    p += pad (s, dataBytes, GC_ARRAY_HEADER_SIZE);
  } else { /* stack */
    GC_stack stack; 
    pointer top, bottom; 
    unsigned int i;
    GC_returnAddress returnAddress; 
    GC_frameLayout *frameLayout;
    GC_frameOffsets frameOffsets;

    assert (STACK_TAG == tag);
    stack = (GC_stack)p;
    bottom = stackBottom (s, stack); 
    top = stackTop (s, stack);
    if (DEBUG) {
      fprintf (stderr, "  bottom = "FMTPTR"  top = "FMTPTR"\n",
               (uintptr_t)bottom, (uintptr_t)top);
    }
    assert (stack->used <= stack->reserved);
    while (top > bottom) {
      /* Invariant: top points just past a "return address". */
      returnAddress = *(GC_returnAddress*) (top - GC_RETURNADDRESS_SIZE);
      if (DEBUG) {
        fprintf (stderr, "  top = "FMTPTR"  return address = "FMTPTR"\n",
                 (uintptr_t)top, returnAddress);
      }
      frameLayout = getFrameLayout (s, returnAddress);
      frameOffsets = frameLayout->offsets;
      top -= frameLayout->numBytes;
      for (i = 0 ; i < frameOffsets[0] ; ++i) {
        if (DEBUG)
          fprintf(stderr, "  offset %"PRIx16"  address "FMTOBJPTR"\n",
                  frameOffsets[i + 1], *(objptr*)(top + frameOffsets[i + 1]));
        maybeCall(f, s, (objptr*)(top + frameOffsets[i + 1]));
      }
    }
    assert(top == bottom);
    p += sizeof (struct GC_stack) + stack->reserved;
  }
  return p;
}

/* foreachObjptrInRange (s, front, back, skipWeaks, f)
 *
 * Apply f to each pointer between front and *back, which should be a
 * contiguous sequence of objects, where front points at the beginning
 * of the first object and *back points just past the end of the last
 * object.  f may increase *back (for example, this is done by
 * forward).  foreachObjptrInRange returns a pointer to the end of
 * the last object it visits.
 *
 * If skipWeaks, then the object pointer in weak objects is skipped.
 */

static inline pointer foreachObjptrInRange (GC_state s, 
                                            pointer front, 
                                            pointer *back,
                                            bool skipWeaks,
                                            GC_pointerFun f) {
  pointer b;

  assert (isAlignedFrontier (s, front));
  if (DEBUG_DETAILED)
    fprintf (stderr, 
             "foreachObjptrInRange  front = "FMTPTR"  *back = "FMTPTR"\n",
             (uintptr_t)front, (uintptr_t)(*back));
  b = *back;
  assert (front <= b);
  while (front < b) {
    while (front < b) {
      assert (isAligned ((uintptr_t)front, GC_MODEL_MINALIGN));
      if (DEBUG_DETAILED)
        fprintf (stderr, 
                 "  front = "FMTPTR"  *back = "FMTPTR"\n",
                 (uintptr_t)front, (uintptr_t)(*back));
      front = foreachObjptrInObject (s, toData (s, front), skipWeaks, f);
    }
    b = *back;
  }
  return front;
}
