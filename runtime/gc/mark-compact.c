/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                 Jonkers Mark-compact Collection                  */
/* ---------------------------------------------------------------- */

/* An object pointer might be larger than a header.
 */ 
void threadInternalCopy (pointer dst, pointer src) {
  size_t count = (OBJPTR_SIZE - GC_HEADER_SIZE) / GC_HEADER_SIZE;
  src = src + GC_HEADER_SIZE * count;

  for (size_t i = 0; i <= count; i++) {
    *((GC_header*)dst) = *((GC_header*)src);
    dst += GC_HEADER_SIZE;
    src -= GC_HEADER_SIZE;
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
  threadInternalCopy ((pointer)(opp), (pointer)(headerp));
  threadInternalCopy ((pointer)(headerp), (pointer)(&opop));
}

/* If p is weak, the object pointer was valid, and points to an unmarked object,
 * then clear the object pointer.
 */
void maybeClearWeak (GC_state s, pointer p) {
  GC_header header;
  GC_header *headerp;
  uint16_t numNonObjptrs, numObjptrs;
  GC_objectTypeTag tag;

  headerp = getHeaderp (p);
  header = *headerp;
  splitHeader(s, *headerp, &tag, NULL, &numNonObjptrs, &numObjptrs);
  if (WEAK_TAG == tag and 1 == numObjptrs) {
    GC_header h2;
    
    if (DEBUG_WEAK)
      fprintf (stderr, "maybeClearWeak ("FMTPTR")  header = "FMTHDR"\n",
               (uintptr_t)p, header);
    h2 = getHeader (objptrToPointer(((GC_weak)p)->objptr, s->heap.start));
    /* If it's unmarked not threaded, clear the weak pointer. */
    if (1 == ((MARK_MASK | 1) & h2)) {
      ((GC_weak)p)->objptr = BOGUS_OBJPTR;
      header = GC_WEAK_GONE_HEADER | MARK_MASK;
      if (DEBUG_WEAK)
        fprintf (stderr, "cleared.  new header = "FMTHDR"\n",
                 header);
      *headerp = header;
    }
  }
}

void updateForwardPointers (GC_state s) {
  pointer back;
  pointer endOfLastMarked;
  pointer front;
  size_t gap;
  GC_header header;
  GC_header *headerp;
  pointer p;
  size_t size;

  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "Update forward pointers.\n");
  front = alignFrontier (s, s->heap.start);
  back = s->heap.start + s->heap.oldGenSize;
  endOfLastMarked = front;
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
  if (1 == (1 & header)) {
    /* It's a header */
    if (MARK_MASK & header) {
      /* It is marked, but has no forward pointers.
       * Thread internal pointers.
       */
thread:
      maybeClearWeak (s, p);
      size = sizeofObject (s, p);
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "threading "FMTPTR" of size %zu\n",
                 (uintptr_t)p, size);
      if ((size_t)(front - endOfLastMarked) >= GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) {
        /* Compress all of the unmarked into one vector.  We require
         * (GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) space to be available
         * because that is the smallest possible array.  You cannot
         * use GC_ARRAY_HEADER_SIZE because even zero-length arrays
         * require an extra word for the forwarding pointer.  If you
         * did use GC_ARRAY_HEADER_SIZE,
         * updateBackwardPointersAndSlide would skip the extra word
         * and be completely busted.
         */
        if (DEBUG_MARK_COMPACT)
          fprintf (stderr, "compressing from "FMTPTR" to "FMTPTR" (length = %zu)\n",
                   (uintptr_t)endOfLastMarked, (uintptr_t)front,
                   (size_t)(front - endOfLastMarked));
        *((GC_arrayCounter*)(endOfLastMarked)) = 0;
        endOfLastMarked = endOfLastMarked + GC_ARRAY_COUNTER_SIZE;
        *((GC_arrayLength*)(endOfLastMarked)) = ((size_t)(front - endOfLastMarked)) - GC_ARRAY_HEADER_SIZE;
        endOfLastMarked = endOfLastMarked + GC_ARRAY_LENGTH_SIZE;
        *((GC_header*)(endOfLastMarked)) = GC_WORD8_VECTOR_HEADER;
      }
      front += size;
      endOfLastMarked = front;
      foreachObjptrInObject (s, p, FALSE, threadInternalObjptr);
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

    assert (0 == (1 & header));
    /* It's a pointer.  This object must be live.  Fix all the forward
     * pointers to it, store its header, then thread its internal
     * pointers.
     */
    new = p - gap;
    newObjptr = pointerToObjptr (new, s->heap.start);
    do {
      pointer cur;
      objptr curObjptr;

      threadInternalCopy ((pointer)(&curObjptr), (pointer)headerp);
      cur = objptrToPointer (curObjptr, s->heap.start);

      threadInternalCopy ((pointer)headerp, cur);
      *((objptr*)cur) = newObjptr;

      header = *headerp;
    } while (0 == (1 & header));
    goto thread;
  }
  assert (FALSE);
done:
  return;
}

void updateBackwardPointersAndSlide (GC_state s) {
  pointer back;
  pointer front;
  size_t gap;
  GC_header header;
  GC_header *headerp;
  pointer p;
  size_t size;

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
  if (1 == (1 & header)) {
    /* It's a header */
    if (MARK_MASK & header) {
      /* It is marked, but has no backward pointers to it.
       * Unmark it.
       */
unmark:
      size = sizeofObject (s, p);
      /* unmark */
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "unmarking "FMTPTR" of size %zu\n",
                 (uintptr_t)p, size);
      *headerp = header & ~MARK_MASK;
      /* slide */
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "sliding "FMTPTR" down %zu\n",
                 (uintptr_t)front, gap);
      GC_memcpy (front, front - gap, size);
      front += size;
      goto updateObject;
    } else {
      /* It's not marked. */
      size = sizeofObject (s, p);
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "skipping "FMTPTR" of size %zu\n",
                 (uintptr_t)p, size);
      gap += size;
      front += size;
      goto updateObject;
    }
  } else {
    pointer new;
    objptr newObjptr;

    assert (0 == (1 & header));
    /* It's a pointer.  This object must be live.  Fix all the
     * backward pointers to it.  Then unmark it.
     */
    new = p - gap;
    newObjptr = pointerToObjptr (new, s->heap.start);
    do {
      pointer cur;
      objptr curObjptr;
      
      threadInternalCopy ((pointer)(&curObjptr), (pointer)headerp);
      cur = objptrToPointer (curObjptr, s->heap.start);

      threadInternalCopy ((pointer)headerp, cur);
      *((objptr*)cur) = newObjptr;

      header = *headerp;
    } while (0 == (1 & header));
    /* The unmarked header will be stored by unmark. */
    goto unmark;
  }
  assert (FALSE);
done:
  s->heap.oldGenSize = front - gap - s->heap.start;
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "oldGenSize = %zu\n", s->heap.oldGenSize);
  return;
}

void majorMarkCompactGC (GC_state s) {
  struct rusage ru_start;

  if (detailedGCTime (s))
    startTiming (&ru_start);
  s->cumulativeStatistics.numMarkCompactGCs++;
  if (DEBUG or s->controls.messages) {
    fprintf (stderr, "Major mark-compact GC.\n");
    fprintf (stderr, "heap = "FMTPTR" of size %zu\n",
             (uintptr_t) s->heap.start, 
             /*uintToCommaString*/(s->heap.size));
  }
  if (s->hashConsDuringGC) {
    s->cumulativeStatistics.bytesHashConsed = 0;
    s->cumulativeStatistics.numHashConsGCs++;
    s->objectHashTable = newHashTable (s);
    foreachGlobalObjptr (s, dfsMarkTrue);
    destroyHashTable (s->objectHashTable);
  } else {
    foreachGlobalObjptr (s, dfsMarkFalse);
  }
  foreachGlobalObjptr (s, threadInternalObjptr);
  updateForwardPointers (s);
  updateBackwardPointersAndSlide (s);
  clearCrossMap (s);
  s->cumulativeStatistics.bytesMarkCompacted += s->heap.oldGenSize;
  s->lastMajorStatistics.kind = GC_MARK_COMPACT;
  if (detailedGCTime (s))
    stopTiming (&ru_start, &s->cumulativeStatistics.ru_gcMarkCompact);
  if (DEBUG or s->controls.messages) {
    fprintf (stderr, "Major mark-compact GC done.\n");
    if (s->hashConsDuringGC)
      bytesHashConsedMessage(s, 
                             s->cumulativeStatistics.bytesHashConsed 
                             + s->heap.oldGenSize);
  }
}
