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

static inline void markGlobalTrue (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap.start);
  mark (s, p, MARK_MODE, TRUE);
}

static inline void markGlobalFalse (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap.start);
  mark (s, p, MARK_MODE, FALSE);
}

static inline void unmarkGlobal (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap.start);
  mark (s, p, UNMARK_MODE, FALSE);
}

static inline void threadInternal (GC_state s, objptr *opp) {
  pointer p;
  GC_header *headerp;

  p = objptrToPointer (*opp, s->heap.start);
  if (FALSE)
    fprintf (stderr, 
             "threadInternal opp = "FMTPTR"  p = "FMTPTR"  header = "FMTHDR"\n",
             (uintptr_t)opp, (uintptr_t)p, getHeader (p));
  headerp = getHeaderp (p);
}

/* static inline void threadInternal (GC_state s, pointer *pp) { */
/*         Header *headerp; */

/*         if (FALSE) */
/*                 fprintf (stderr, "threadInternal pp = 0x%08x  *pp = 0x%08x  header = 0x%08x\n", */
/*                                 (uint)pp, *(uint*)pp, (uint)GC_getHeader (*pp)); */
/*         headerp = GC_getHeaderp (*pp); */
/*         *(Header*)pp = *headerp; */
/*         *headerp = (Header)pp; */
/* } */

/* /\* If p is weak, the object pointer was valid, and points to an unmarked object, */
/*  * then clear the object pointer. */
/*  *\/ */
/* static inline void maybeClearWeak (GC_state s, pointer p) { */
/*         Bool hasIdentity; */
/*         Header header; */
/*         Header *headerp; */
/*         uint numPointers; */
/*         uint numNonPointers; */
/*         uint tag; */

/*         headerp = GC_getHeaderp (p); */
/*         header = *headerp; */
/*         SPLIT_HEADER(); */
/*         if (WEAK_TAG == tag and 1 == numPointers) {  */
/*                 Header h2; */

/*                 if (DEBUG_WEAK) */
/*                         fprintf (stderr, "maybeClearWeak (0x%08x)  header = 0x%08x\n", */
/*                                         (uint)p, (uint)header); */
/*                 h2 = GC_getHeader (((GC_weak)p)->object); */
/*                 /\* If it's unmarked not threaded, clear the weak pointer. *\/ */
/*                 if (1 == ((MARK_MASK | 1) & h2)) { */
/*                         ((GC_weak)p)->object = (pointer)BOGUS_POINTER; */
/*                         header = WEAK_GONE_HEADER | MARK_MASK; */
/*                         if (DEBUG_WEAK) */
/*                                 fprintf (stderr, "cleared.  new header = 0x%08x\n", */
/*                                                 (uint)header); */
/*                         *headerp = header; */
/*                 } */
/*         } */
/* } */

/* static void updateForwardPointers (GC_state s) { */
/*         pointer back; */
/*         pointer front; */
/*         uint gap; */
/*         pointer endOfLastMarked; */
/*         Header header; */
/*         Header *headerp; */
/*         pointer p; */
/*         uint size; */

/*         if (DEBUG_MARK_COMPACT) */
/*                 fprintf (stderr, "Update forward pointers.\n"); */
/*         front = alignFrontier (s, s->heap.start); */
/*         back = s->heap.start + s->oldGenSize; */
/*         endOfLastMarked = front; */
/*         gap = 0; */
/* updateObject: */
/*         if (DEBUG_MARK_COMPACT) */
/*                 fprintf (stderr, "updateObject  front = 0x%08x  back = 0x%08x\n", */
/*                                 (uint)front, (uint)back); */
/*         if (front == back) */
/*                 goto done; */
/*         headerp = (Header*)front; */
/*         header = *headerp; */
/*         if (0 == header) { */
/*                 /\* We're looking at an array.  Move to the header. *\/ */
/*                 p = front + 3 * WORD_SIZE; */
/*                 headerp = (Header*)(p - WORD_SIZE); */
/*                 header = *headerp; */
/*         } else  */
/*                 p = front + WORD_SIZE; */
/*         if (1 == (1 & header)) { */
/*                 /\* It's a header *\/ */
/*                 if (MARK_MASK & header) { */
/*                         /\* It is marked, but has no forward pointers.  */
/*                          * Thread internal pointers. */
/*                          *\/ */
/* thread: */
/*                         maybeClearWeak (s, p); */
/*                         size = objectSize (s, p); */
/*                         if (DEBUG_MARK_COMPACT) */
/*                                 fprintf (stderr, "threading 0x%08x of size %u\n",  */
/*                                                 (uint)p, size); */
/*                         if (front - endOfLastMarked >= 4 * WORD_SIZE) { */
/*                                 /\* Compress all of the unmarked into one string. */
/*                                  * We require 4 * WORD_SIZE space to be available */
/*                                  * because that is the smallest possible array. */
/*                                  * You cannot use 3 * WORD_SIZE because even */
/*                                  * zero-length arrays require an extra word for */
/*                                  * the forwarding pointer.  If you did use */
/*                                  * 3 * WORD_SIZE, updateBackwardPointersAndSlide */
/*                                  * would skip the extra word and be completely */
/*                                  * busted. */
/*                                  *\/ */
/*                                 if (DEBUG_MARK_COMPACT) */
/*                                         fprintf (stderr, "compressing from 0x%08x to 0x%08x (length = %u)\n", */
/*                                                         (uint)endOfLastMarked, */
/*                                                         (uint)front, */
/*                                                         front - endOfLastMarked); */
/*                                 *(uint*)endOfLastMarked = 0; */
/*                                 *(uint*)(endOfLastMarked + WORD_SIZE) =  */
/*                                         front - endOfLastMarked - 3 * WORD_SIZE; */
/*                                 *(uint*)(endOfLastMarked + 2 * WORD_SIZE) = */
/*                                         GC_objectHeader (STRING_TYPE_INDEX); */
/*                         } */
/*                         front += size; */
/*                         endOfLastMarked = front; */
/*                         foreachPointerInObject (s, p, FALSE, threadInternal); */
/*                         goto updateObject; */
/*                 } else { */
/*                         /\* It's not marked. *\/ */
/*                         size = objectSize (s, p); */
/*                         gap += size; */
/*                         front += size; */
/*                         goto updateObject; */
/*                 } */
/*         } else { */
/*                 pointer new; */

/*                 assert (0 == (3 & header)); */
/*                 /\* It's a pointer.  This object must be live.  Fix all the */
/*                  * forward pointers to it, store its header, then thread */
/*                  * its internal pointers. */
/*                  *\/ */
/*                 new = p - gap; */
/*                 do { */
/*                         pointer cur; */

/*                         cur = (pointer)header; */
/*                         header = *(word*)cur; */
/*                         *(word*)cur = (word)new; */
/*                 } while (0 == (1 & header)); */
/*                 *headerp = header; */
/*                 goto thread; */
/*         } */
/*         assert (FALSE); */
/* done: */
/*         return; */
/* } */

/* static void updateBackwardPointersAndSlide (GC_state s) { */
/*         pointer back; */
/*         pointer front; */
/*         uint gap; */
/*         Header header; */
/*         pointer p; */
/*         uint size; */

/*         if (DEBUG_MARK_COMPACT) */
/*                 fprintf (stderr, "Update backward pointers and slide.\n"); */
/*         front = alignFrontier (s, s->heap.start); */
/*         back = s->heap.start + s->oldGenSize; */
/*         gap = 0; */
/* updateObject: */
/*         if (DEBUG_MARK_COMPACT) */
/*                 fprintf (stderr, "updateObject  front = 0x%08x  back = 0x%08x\n", */
/*                                 (uint)front, (uint)back); */
/*         if (front == back) */
/*                 goto done; */
/*         header = *(word*)front; */
/*         if (0 == header) { */
/*                 /\* We're looking at an array.  Move to the header. *\/ */
/*                 p = front + 3 * WORD_SIZE; */
/*                 header = *(Header*)(p - WORD_SIZE); */
/*         } else  */
/*                 p = front + WORD_SIZE; */
/*         if (1 == (1 & header)) { */
/*                 /\* It's a header *\/ */
/*                 if (MARK_MASK & header) { */
/*                         /\* It is marked, but has no backward pointers to it. */
/*                          * Unmark it. */
/*                          *\/ */
/* unmark: */
/*                         *GC_getHeaderp (p) = header & ~MARK_MASK; */
/*                         size = objectSize (s, p); */
/*                         if (DEBUG_MARK_COMPACT) */
/*                                 fprintf (stderr, "unmarking 0x%08x of size %u\n",  */
/*                                                 (uint)p, size); */
/*                         /\* slide *\/ */
/*                         if (DEBUG_MARK_COMPACT) */
/*                                 fprintf (stderr, "sliding 0x%08x down %u\n", */
/*                                                 (uint)front, gap); */
/*                         copy (front, front - gap, size); */
/*                         front += size; */
/*                         goto updateObject; */
/*                 } else { */
/*                         /\* It's not marked. *\/ */
/*                         size = objectSize (s, p); */
/*                         if (DEBUG_MARK_COMPACT) */
/*                                 fprintf (stderr, "skipping 0x%08x of size %u\n", */
/*                                                 (uint)p, size); */
/*                         gap += size; */
/*                         front += size; */
/*                         goto updateObject; */
/*                 } */
/*         } else { */
/*                 pointer new; */

/*                 /\* It's a pointer.  This object must be live.  Fix all the */
/*                  * backward pointers to it.  Then unmark it. */
/*                  *\/ */
/*                 new = p - gap; */
/*                 do { */
/*                         pointer cur; */

/*                         assert (0 == (3 & header)); */
/*                         cur = (pointer)header; */
/*                         header = *(word*)cur; */
/*                         *(word*)cur = (word)new; */
/*                 } while (0 == (1 & header)); */
/*                 /\* The header will be stored by unmark. *\/ */
/*                 goto unmark; */
/*         } */
/*         assert (FALSE); */
/* done: */
/*         s->oldGenSize = front - gap - s->heap.start; */
/*         if (DEBUG_MARK_COMPACT) */
/*                 fprintf (stderr, "bytesLive = %u\n", s->bytesLive); */
/*         return; */
/* } */

static void majorMarkCompactGC (GC_state s) {
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
    foreachGlobalObjptr (s, markGlobalTrue);
    destroyHashTable (s->objectHashTable);
  } else {
    foreachGlobalObjptr (s, markGlobalFalse);
  }
/*   foreachGlobal (s, threadInternal); */
/*   updateForwardPointers (s); */
/*   updateBackwardPointersAndSlide (s); */
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
