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
static pointer newObject (GC_state s,
                          GC_header header,
                          size_t bytesRequested,
                          bool allocInOldGen) {
  pointer frontier;
  pointer result;

  assert (isAligned (bytesRequested, s->alignment));
  assert (allocInOldGen
          ? heapHasBytesFree (s, bytesRequested, 0)
          : heapHasBytesFree (s, 0, bytesRequested));
  if (allocInOldGen) {
    frontier = s->heap.start + s->heap.oldGenSize;
    s->heap.oldGenSize += bytesRequested;
    s->cumulativeStatistics.bytesAllocated += bytesRequested;
  } else {
    if (DEBUG_DETAILED)
      fprintf (stderr, "frontier changed from "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)s->frontier, 
               (uintptr_t)(s->frontier + bytesRequested));
    frontier = s->frontier;
    s->frontier += bytesRequested;
  }
  GC_profileAllocInc (s, bytesRequested);
  *(GC_header*)(frontier) = header;
  result = frontier + GC_NORMAL_HEADER_SIZE;
  if (DEBUG)
    fprintf (stderr, FMTPTR " = newObject ("FMTHDR", %zd, %s)\n",
             (uintptr_t)result,
             header, 
             bytesRequested,
             boolToString (allocInOldGen));
  return result;
}

static GC_stack newStack (GC_state s, 
                          size_t reserved, 
                          bool allocInOldGen) {
  GC_stack stack;

  reserved = stackReserved (s, reserved);
  if (reserved > s->cumulativeStatistics.maxStackSizeSeen)
    s->cumulativeStatistics.maxStackSizeSeen = reserved;
  stack = (GC_stack) newObject (s, GC_STACK_HEADER, 
                                stackNumBytes (s, reserved),
                                allocInOldGen);
  stack->reserved = reserved;
  stack->used = 0;
  if (DEBUG_STACKS)
    fprintf (stderr, FMTPTR " = newStack (%zd)\n", 
             (uintptr_t)stack, 
             reserved);
  return stack;
}
