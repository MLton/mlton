/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void callIfIsObjptr (GC_state s, GC_foreachObjptrFun f, objptr *opp) {
  if (isObjptr (*opp))
    f (s, opp);
}

/* foreachGlobalObjptr (s, f)
 * 
 * Apply f to each global object pointer into the heap. 
 */
void foreachGlobalObjptr (GC_state s, GC_foreachObjptrFun f) {
  for (unsigned int i = 0; i < s->globalsLength; ++i) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "foreachGlobal %u\n", i);
    callIfIsObjptr (s, f, &s->globals [i]);
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "foreachGlobal threads\n");
  callIfIsObjptr (s, f, &s->callFromCHandlerThread);
  callIfIsObjptr (s, f, &s->currentThread);
  callIfIsObjptr (s, f, &s->savedThread);
  callIfIsObjptr (s, f, &s->signalHandlerThread);
}


/* foreachObjptrInObject (s, p, f, skipWeaks) 
 * 
 * Applies f to each object pointer in the object pointed to by p.
 * Returns pointer to the end of object, i.e. just past object.
 *
 * If skipWeaks, then the object pointer in weak objects is skipped.
 */
pointer foreachObjptrInObject (GC_state s, pointer p,
                               GC_foreachObjptrFun f, bool skipWeaks) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  header = getHeader (p);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  if (DEBUG_DETAILED)
    fprintf (stderr, 
             "foreachObjptrInObject ("FMTPTR")"
             "  header = "FMTHDR
             "  tag = %s"
             "  bytesNonObjptrs = %d"
             "  numObjptrs = %d\n", 
             (uintptr_t)p, header, objectTypeTagToString (tag), 
             bytesNonObjptrs, numObjptrs);
  if (NORMAL_TAG == tag) {
    p += bytesNonObjptrs;
    pointer max = p + (numObjptrs * OBJPTR_SIZE);
    /* Apply f to all internal pointers. */
    for ( ; p < max; p += OBJPTR_SIZE) {
      if (DEBUG_DETAILED)
        fprintf (stderr, 
                 "  p = "FMTPTR"  *p = "FMTOBJPTR"\n",
                 (uintptr_t)p, *(objptr*)p);
      callIfIsObjptr (s, f, (objptr*)p);
    }
  } else if (WEAK_TAG == tag) {
    p += bytesNonObjptrs;
    if (1 == numObjptrs) {
      if (not skipWeaks)
        callIfIsObjptr (s, f, (objptr*)p);
      p += OBJPTR_SIZE;
    }
  } else if (ARRAY_TAG == tag) {
    size_t bytesPerElement;
    size_t dataBytes;
    pointer last;
    GC_arrayLength numElements;

    numElements = getArrayLength (p);
    bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    dataBytes = numElements * bytesPerElement;
    if (0 == numObjptrs) {
      /* No objptrs to process. */
      ;
    } else {
      last = p + dataBytes;
      if (0 == bytesNonObjptrs)
        /* Array with only pointers. */
        for ( ; p < last; p += OBJPTR_SIZE)
          callIfIsObjptr (s, f, (objptr*)p);
      else {
        /* Array with a mix of pointers and non-pointers. */
        size_t bytesObjptrs;

        bytesObjptrs = numObjptrs * OBJPTR_SIZE;

        /* For each array element. */
        for ( ; p < last; ) {
          pointer next;

          /* Skip the non-pointers. */
          p += bytesNonObjptrs;
          next = p + bytesObjptrs;
          /* For each internal pointer. */
          for ( ; p < next; p += OBJPTR_SIZE) 
            callIfIsObjptr (s, f, (objptr*)p);
        }
      }
      assert (p == last);
      p -= dataBytes;
    }
    p += alignWithExtra (s, dataBytes, GC_ARRAY_METADATA_SIZE);
  } else { /* stack */
    GC_stack stack; 
    pointer top, bottom; 
    unsigned int i;
    GC_returnAddress returnAddress; 
    GC_frameLayout frameLayout;
    GC_frameOffsets frameOffsets;

    assert (STACK_TAG == tag);
    stack = (GC_stack)p;
    bottom = getStackBottom (s, stack); 
    top = getStackTop (s, stack);
    if (DEBUG) {
      fprintf (stderr, "  bottom = "FMTPTR"  top = "FMTPTR"\n",
               (uintptr_t)bottom, (uintptr_t)top);
    }
    assert (stack->used <= stack->reserved);
    while (top > bottom) {
      /* Invariant: top points just past a "return address". */
      returnAddress = *((GC_returnAddress*)(top - GC_RETURNADDRESS_SIZE));
      if (DEBUG) {
        fprintf (stderr, "  top = "FMTPTR"  return address = "FMTRA"\n",
                 (uintptr_t)top, returnAddress);
      }
      frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
      frameOffsets = frameLayout->offsets;
      top -= frameLayout->size;
      for (i = 0 ; i < frameOffsets[0] ; ++i) {
        if (DEBUG)
          fprintf(stderr, "  offset %"PRIx16"  address "FMTOBJPTR"\n",
                  frameOffsets[i + 1], *(objptr*)(top + frameOffsets[i + 1]));
        callIfIsObjptr (s, f, (objptr*)(top + frameOffsets[i + 1]));
      }
    }
    assert(top == bottom);
    p += sizeof (struct GC_stack) + stack->reserved;
  }
  return p;
}

/* foreachObjptrInRange (s, front, back, f, skipWeaks)
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

pointer foreachObjptrInRange (GC_state s, pointer front, pointer *back,
                              GC_foreachObjptrFun f, bool skipWeaks) {
  pointer b;

  assert (isFrontierAligned (s, front));
  if (DEBUG_DETAILED)
    fprintf (stderr, 
             "foreachObjptrInRange  front = "FMTPTR"  *back = "FMTPTR"\n",
             (uintptr_t)front, (uintptr_t)(*back));
  b = *back;
  assert (front <= b);
  while (front < b) {
    while (front < b) {
      assert (isAligned ((size_t)front, GC_MODEL_MINALIGN));
      if (DEBUG_DETAILED)
        fprintf (stderr, 
                 "  front = "FMTPTR"  *back = "FMTPTR"\n",
                 (uintptr_t)front, (uintptr_t)(*back));
      pointer p = advanceToObjectData (s, front);
      assert (isAligned ((size_t)p, s->alignment));
      front = foreachObjptrInObject (s, p, f, skipWeaks);
    }
    b = *back;
  }
  return front;
}


/* Apply f to the frame index of each frame in the current thread's stack. */
void foreachStackFrame (GC_state s, GC_foreachStackFrameFun f) {
  pointer bottom;
  GC_frameIndex findex;
  GC_frameLayout layout;
  GC_returnAddress returnAddress;
  pointer top;

  if (DEBUG_PROFILE)
    fprintf (stderr, "foreachStackFrame\n");
  bottom = getStackBottom (s, getStackCurrent(s));
  if (DEBUG_PROFILE)
    fprintf (stderr, "  bottom = "FMTPTR"  top = "FMTPTR".\n",
             (uintptr_t)bottom, (uintptr_t)s->stackTop);
  for (top = s->stackTop; top > bottom; top -= layout->size) {
    returnAddress = *((GC_returnAddress*)(top - GC_RETURNADDRESS_SIZE));
    findex = getFrameIndexFromReturnAddress (s, returnAddress);
    if (DEBUG_PROFILE)
      fprintf (stderr, "top = "FMTPTR"  findex = "FMTFI"\n",
               (uintptr_t)top, findex);
    unless (findex < s->frameLayoutsLength)
      die ("top = "FMTPTR"  returnAddress = "FMTRA"  findex = "FMTFI"\n",
           (uintptr_t)top, (uintptr_t)returnAddress, findex);
    f (s, findex);
    layout = &(s->frameLayouts[findex]);
    assert (layout->size > 0);
  }
  if (DEBUG_PROFILE)
    fprintf (stderr, "done foreachStackFrame\n");
}
