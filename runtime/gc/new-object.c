/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* newObject (s, header, bytesRequested, allocInOldGen)
 *
 * Allocate a new object in the heap.
 * bytesRequested includes the size of the header.
 */
pointer newObject (GC_state s,
                   GC_header header,
                   size_t bytesRequested,
                   bool allocInOldGen) {
  pointer frontier;
  pointer result;

  assert (isAligned (bytesRequested, s->alignment));
  assert (allocInOldGen
          ? hasHeapBytesFree (s, bytesRequested, 0)
          : hasHeapBytesFree (s, 0, bytesRequested));
  if (allocInOldGen) {
    /* NB you must have exclusive access to the runtime state
       if you are allocating in the older generation! */
    frontier = s->heap->start + s->heap->oldGenSize;
    s->heap->oldGenSize += bytesRequested;
    s->cumulativeStatistics->bytesAllocated += bytesRequested;
  } else {
    if (DEBUG_DETAILED)
      fprintf (stderr, "frontier changed from "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)s->frontier, 
               (uintptr_t)(s->frontier + bytesRequested));
    frontier = s->frontier;
    s->frontier += bytesRequested;
  }
  GC_profileAllocInc (s, bytesRequested);
  *((GC_header*)frontier) = header;
  result = frontier + GC_NORMAL_HEADER_SIZE;
  assert (isAligned ((size_t)result, s->alignment));
  if (DEBUG)
    fprintf (stderr, FMTPTR " = newObject ("FMTHDR", %zu, %s)\n",
             (uintptr_t)result,
             header, 
             bytesRequested,
             boolToString (allocInOldGen));
  return result;
}

GC_stack newStack (GC_state s, 
                   size_t reserved, 
                   bool allocInOldGen) {
  GC_stack stack;

  reserved = alignStackReserved (s, reserved);
  /* XXX unsafe concurrent access */
  if (reserved > s->cumulativeStatistics->maxStackSizeSeen)
    s->cumulativeStatistics->maxStackSizeSeen = reserved;
  stack = (GC_stack)(newObject (s, GC_STACK_HEADER, 
                                sizeofStackWithHeaderAligned (s, reserved),
                                allocInOldGen));
  stack->reserved = reserved;
  stack->used = 0;
  if (DEBUG_STACKS)
    fprintf (stderr, FMTPTR " = newStack (%zu)\n", 
             (uintptr_t)stack, 
             reserved);
  return stack;
}

GC_thread newThread (GC_state s, size_t reserved) {
  GC_stack stack;
  GC_thread thread;
  pointer res;

  ensureHasHeapBytesFree (s, 0, sizeofStackWithHeaderAligned (s, reserved) + sizeofThread (s));
  stack = newStack (s, reserved, FALSE);
  res = newObject (s, GC_THREAD_HEADER, 
                   sizeofThread (s), 
                   FALSE);
  thread = (GC_thread)(res + offsetofThread (s));
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->stack = pointerToObjptr((pointer)stack, s->heap->start);
  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = newThreadOfSize (%zu)\n",
             (uintptr_t)thread, reserved);;
  return thread;
}

static inline void setFrontier (GC_state s, pointer p, 
                                __attribute__ ((unused)) size_t bytes) {
  p = alignFrontier (s, p);
  assert ((size_t)(p - s->frontier) <= bytes);
  GC_profileAllocInc (s, p - s->frontier);
  /* XXX unsafe concurrent access */
  s->cumulativeStatistics->bytesAllocated += p - s->frontier;
  s->frontier = p;
  assert (s->frontier <= s->limitPlusSlop);
  assert (s->start <= s->frontier);
}
