/* Copyright (C) 2010,2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                 Jonkers Mark-compact Collection                  */
/* ---------------------------------------------------------------- */

void copyForThreadInternal (pointer dst, pointer src) {
  if (FALSE)
    fprintf (stderr,
             "copyForThreadInternal dst = "FMTPTR"  src = "FMTPTR"\n",
             (uintptr_t)dst, (uintptr_t)src);
  if (OBJPTR_SIZE > GC_HEADER_SIZE) {
    size_t count;

    assert (0 == (OBJPTR_SIZE % GC_HEADER_SIZE));
    count = (OBJPTR_SIZE - GC_HEADER_SIZE) / GC_HEADER_SIZE;
    src = src + GC_HEADER_SIZE * count;

    for (size_t i = 0; i <= count; i++) {
      *((GC_header*)dst) = *((GC_header*)src);
      dst += GC_HEADER_SIZE;
      src -= GC_HEADER_SIZE;
    }
  } else if (GC_HEADER_SIZE > OBJPTR_SIZE) {
    size_t count;

    assert (0 == (GC_HEADER_SIZE % OBJPTR_SIZE));
    count = (GC_HEADER_SIZE - OBJPTR_SIZE) / OBJPTR_SIZE;
    dst = dst + OBJPTR_SIZE * count;

    for (size_t i = 0; i <= count; i++) {
      *((objptr*)dst) = *((objptr*)src);
      dst -= OBJPTR_SIZE;
      src += OBJPTR_SIZE;
    }
  } else /* (GC_HEADER_SIZE == OBJPTR_SIZE) */ {
    *((GC_header*)dst) = *((GC_header*)src);
  }
}

void threadInternalObjptr (GC_state s, objptr *opp) {
  objptr opop;
  pointer p;
  GC_header *headerp;

  opop = pointerToObjptr ((pointer)opp, s->heap.start);
  p = objptrToPointer (*opp, s->heap.start);
  if (FALSE)
    fprintf (stderr,
             "threadInternal opp = "FMTPTR"  p = "FMTPTR"  header = "FMTHDR"\n",
             (uintptr_t)opp, (uintptr_t)p, getHeader (p));
  headerp = getHeaderp (p);
  copyForThreadInternal ((pointer)(opp), (pointer)(headerp));
  copyForThreadInternal ((pointer)(headerp), (pointer)(&opop));
}

/* If the object pointer is valid, and points to an unmarked object,
 * then clear the object pointer.
 */
void updateWeaksForMarkCompact (GC_state s) {
  pointer p;
  GC_weak w;

  for (w = s->weaks; w != NULL; w = w->link) {
    assert (BOGUS_OBJPTR != w->objptr);

    if (DEBUG_WEAK)
      fprintf (stderr, "updateWeaksForMarkCompact  w = "FMTPTR"  ", (uintptr_t)w);
    p = objptrToPointer(w->objptr, s->heap.start);
    /* If it's unmarked, clear the weak pointer. */
    if (isPointerMarked(p)) {
      if (DEBUG_WEAK)
        fprintf (stderr, "not cleared\n");
    } else {
      if (DEBUG_WEAK)
        fprintf (stderr, "cleared\n");
      *(getHeaderp((pointer)w - offsetofWeak (s))) = GC_WEAK_GONE_HEADER | MARK_MASK;
      w->objptr = BOGUS_OBJPTR;
    }
  }
  s->weaks = NULL;
}

void updateForwardPointersForMarkCompact (GC_state s, GC_stack currentStack) {
  pointer back;
  pointer endOfLastMarked;
  pointer front;
  size_t gap;
  GC_header header;
  GC_header *headerp;
  pointer p;
  size_t size, skipFront, skipGap;

  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "Update forward pointers.\n");
  front = alignFrontier (s, s->heap.start);
  back = s->heap.start + s->heap.oldGenSize;
  gap = 0;
  endOfLastMarked = front;
updateObject:
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "updateObject  front = "FMTPTR"  back = "FMTPTR"\n",
             (uintptr_t)front, (uintptr_t)back);
  if (front == back)
    goto done;
  p = advanceToObjectData (s, front);
  headerp = getHeaderp (p);
  header = *headerp;
  if (GC_VALID_HEADER_MASK & header) {
    /* It's a header */
    if (MARK_MASK & header) {
      /* It is marked, but has no forward pointers.
       * Thread internal pointers.
       */
thread:
      assert (GC_VALID_HEADER_MASK & header);
      assert (MARK_MASK & header);

      size_t metaDataBytes, objectBytes;
      GC_objectTypeTag tag;
      uint16_t bytesNonObjptrs, numObjptrs;

      assert (header == getHeader (p));
      splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

      /* Compute the space taken by the header and object body. */
      if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
        metaDataBytes = GC_NORMAL_METADATA_SIZE;
        objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
        skipFront = 0;
        skipGap = 0;
      } else if (ARRAY_TAG == tag) {
        metaDataBytes = GC_ARRAY_METADATA_SIZE;
        objectBytes = sizeofArrayNoMetaData (s, getArrayLength (p),
                                             bytesNonObjptrs, numObjptrs);
        skipFront = 0;
        skipGap = 0;
      } else { /* Stack. */
        bool current;
        size_t reservedNew, reservedOld;
        GC_stack stack;

        assert (STACK_TAG == tag);
        metaDataBytes = GC_STACK_METADATA_SIZE;
        stack = (GC_stack)p;
        current = currentStack == stack;

        reservedOld = stack->reserved;
        reservedNew = sizeofStackShrinkReserved (s, stack, current);
        objectBytes = sizeof (struct GC_stack) + stack->used;
        skipFront = reservedOld - stack->used;
        skipGap = reservedOld - reservedNew;
      }
      size = metaDataBytes + objectBytes;
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "threading "FMTPTR" of size %"PRIuMAX"\n",
                 (uintptr_t)p, (uintmax_t)size);
      if ((size_t)(front - endOfLastMarked) >= GC_ARRAY_METADATA_SIZE) {
        pointer newArray = endOfLastMarked;
        /* Compress all of the unmarked into one vector.  We require
         * GC_ARRAY_METADATA_SIZE space to be available because that is
         * the smallest possible array.
         */
        if (DEBUG_MARK_COMPACT)
          fprintf (stderr, "compressing from "FMTPTR" to "FMTPTR" (length = %"PRIuMAX")\n",
                   (uintptr_t)endOfLastMarked, (uintptr_t)front,
                   (uintmax_t)(front - endOfLastMarked));
        *((GC_arrayCounter*)(newArray)) = 0;
        newArray += GC_ARRAY_COUNTER_SIZE;
        *((GC_arrayLength*)(newArray)) =
          ((size_t)(front - endOfLastMarked)) - GC_ARRAY_METADATA_SIZE;
        newArray += GC_ARRAY_LENGTH_SIZE;
        *((GC_header*)(newArray)) = GC_WORD8_VECTOR_HEADER;
      }
      gap += skipGap;
      front += size + skipFront;
      endOfLastMarked = front;
      foreachObjptrInObject (s, p, threadInternalObjptr, FALSE);
      goto updateObject;
    } else {
      /* It's not marked. */
      size = sizeofObject (s, p);
      gap += size;
      front += size;
      goto updateObject;
    }
  } else {
    pointer new;
    objptr newObjptr;

    assert (not (GC_VALID_HEADER_MASK & header));
    /* It's a pointer.  This object must be live.  Fix all the forward
     * pointers to it, store its header, then thread its internal
     * pointers.
     */
    new = p - gap;
    newObjptr = pointerToObjptr (new, s->heap.start);
    do {
      pointer cur;
      objptr curObjptr;

      copyForThreadInternal ((pointer)(&curObjptr), (pointer)headerp);
      cur = objptrToPointer (curObjptr, s->heap.start);

      copyForThreadInternal ((pointer)headerp, cur);
      *((objptr*)cur) = newObjptr;

      header = *headerp;
    } while (0 == (1 & header));
    goto thread;
  }
  assert (FALSE);
done:
  return;
}

void updateBackwardPointersAndSlideForMarkCompact (GC_state s, GC_stack currentStack) {
  pointer back;
  pointer front;
  size_t gap;
  GC_header header;
  GC_header *headerp;
  pointer p;
  size_t size, skipFront, skipGap;

  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "Update backward pointers and slide.\n");
  front = alignFrontier (s, s->heap.start);
  back = s->heap.start + s->heap.oldGenSize;
  gap = 0;
updateObject:
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "updateObject  front = "FMTPTR"  back = "FMTPTR"\n",
             (uintptr_t)front, (uintptr_t)back);
  if (front == back)
    goto done;
  p = advanceToObjectData (s, front);
  headerp = getHeaderp (p);
  header = *headerp;
  if (GC_VALID_HEADER_MASK & header) {
    /* It's a header */
    if (MARK_MASK & header) {
      /* It is marked, but has no backward pointers to it.
       * Unmark it.
       */
unmark:
      assert (GC_VALID_HEADER_MASK & header);
      assert (MARK_MASK & header);

      size_t metaDataBytes, objectBytes;
      GC_objectTypeTag tag;
      uint16_t bytesNonObjptrs, numObjptrs;

      assert (header == getHeader (p));
      splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

      /* Compute the space taken by the header and object body. */
      if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
        metaDataBytes = GC_NORMAL_METADATA_SIZE;
        objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
        skipFront = 0;
        skipGap = 0;
      } else if (ARRAY_TAG == tag) {
        metaDataBytes = GC_ARRAY_METADATA_SIZE;
        objectBytes = sizeofArrayNoMetaData (s, getArrayLength (p),
                                             bytesNonObjptrs, numObjptrs);
        skipFront = 0;
        skipGap = 0;
      } else { /* Stack. */
        bool current;
        size_t reservedNew, reservedOld;
        GC_stack stack;

        assert (STACK_TAG == tag);
        metaDataBytes = GC_STACK_METADATA_SIZE;
        stack = (GC_stack)p;
        current = currentStack == stack;

        reservedOld = stack->reserved;
        reservedNew = sizeofStackShrinkReserved (s, stack, current);
        if (reservedNew < stack->reserved) {
          if (DEBUG_STACKS or s->controls.messages)
            fprintf (stderr,
                     "[GC: Shrinking stack of size %s bytes to size %s bytes, using %s bytes.]\n",
                     uintmaxToCommaString(stack->reserved),
                     uintmaxToCommaString(reservedNew),
                     uintmaxToCommaString(stack->used));
          stack->reserved = reservedNew;
        }
        objectBytes = sizeof (struct GC_stack) + stack->used;
        skipFront = reservedOld - stack->used;
        skipGap = reservedOld - reservedNew;
      }
      size = metaDataBytes + objectBytes;
      /* unmark */
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "unmarking "FMTPTR" of size %"PRIuMAX"\n",
                 (uintptr_t)p, (uintmax_t)size);
      *headerp = header & ~MARK_MASK;
      /* slide */
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "sliding "FMTPTR" down %"PRIuMAX" to "FMTPTR"\n",
                 (uintptr_t)front, (uintmax_t)gap, (uintptr_t)(front - gap));
      GC_memmove (front, front - gap, size);
      gap += skipGap;
      front += size + skipFront;
      goto updateObject;
    } else {
      /* It's not marked. */
      size = sizeofObject (s, p);
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "skipping "FMTPTR" of size %"PRIuMAX"\n",
                 (uintptr_t)p, (uintmax_t)size);
      gap += size;
      front += size;
      goto updateObject;
    }
  } else {
    pointer new;
    objptr newObjptr;

    assert (not (GC_VALID_HEADER_MASK & header));
    /* It's a pointer.  This object must be live.  Fix all the
     * backward pointers to it.  Then unmark it.
     */
    new = p - gap;
    newObjptr = pointerToObjptr (new, s->heap.start);
    do {
      pointer cur;
      objptr curObjptr;

      copyForThreadInternal ((pointer)(&curObjptr), (pointer)headerp);
      cur = objptrToPointer (curObjptr, s->heap.start);

      copyForThreadInternal ((pointer)headerp, cur);
      *((objptr*)cur) = newObjptr;

      header = *headerp;
    } while (0 == (1 & header));
    /* The unmarked header will be stored by unmark. */
    goto unmark;
  }
  assert (FALSE);
done:
  s->heap.oldGenSize = (size_t)((front - gap) - s->heap.start);
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "oldGenSize = %"PRIuMAX"\n",
             (uintmax_t)s->heap.oldGenSize);
  return;
}

void majorMarkCompactGC (GC_state s) {
  size_t bytesHashConsed;
  size_t bytesMarkCompacted;
  GC_stack currentStack;
  struct rusage ru_start;

  if (detailedGCTime (s))
    startTiming (&ru_start);
  s->cumulativeStatistics.numMarkCompactGCs++;
  if (DEBUG or s->controls.messages) {
    fprintf (stderr,
             "[GC: Starting major mark-compact;]\n");
    fprintf (stderr,
             "[GC:\theap at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(s->heap.start),
             uintmaxToCommaString(s->heap.size));
  }
  currentStack = getStackCurrent (s);
  if (s->hashConsDuringGC) {
    s->lastMajorStatistics.bytesHashConsed = 0;
    s->cumulativeStatistics.numHashConsGCs++;
    s->objectHashTable = allocHashTable (s);
    foreachGlobalObjptr (s, dfsMarkWithHashConsWithLinkWeaks);
    freeHashTable (s->objectHashTable);
  } else {
    foreachGlobalObjptr (s, dfsMarkWithoutHashConsWithLinkWeaks);
  }
  updateWeaksForMarkCompact (s);
  foreachGlobalObjptr (s, threadInternalObjptr);
  updateForwardPointersForMarkCompact (s, currentStack);
  updateBackwardPointersAndSlideForMarkCompact (s, currentStack);
  bytesHashConsed = s->lastMajorStatistics.bytesHashConsed;
  s->cumulativeStatistics.bytesHashConsed += bytesHashConsed;
  bytesMarkCompacted = s->heap.oldGenSize;
  s->cumulativeStatistics.bytesMarkCompacted += bytesMarkCompacted;
  s->lastMajorStatistics.kind = GC_MARK_COMPACT;
  if (detailedGCTime (s))
    stopTiming (&ru_start, &s->cumulativeStatistics.ru_gcMarkCompact);
  if (DEBUG or s->controls.messages) {
    fprintf (stderr,
             "[GC: Finished major mark-compact; mark compacted %s bytes.]\n",
             uintmaxToCommaString(bytesMarkCompacted));
    if (s->hashConsDuringGC)
      printBytesHashConsedMessage(bytesHashConsed, 
                                  bytesHashConsed + bytesMarkCompacted);
  }
}
