/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if ASSERT

static bool invariant (GC_state s) {
  if (DEBUG)
    fprintf (stderr, "invariant\n");
  // assert (ratiosOk (s));
  /* Frame layouts */
  for (unsigned int i = 0; i < s->frameLayoutsLength; ++i) {
    GC_frameLayout *layout;
    
    layout = &(s->frameLayouts[i]);
    if (layout->size > 0) {
      GC_frameOffsets offsets;
      
      assert (layout->size <= s->maxFrameSize);
      offsets = layout->offsets;
      /* No longer correct, since handler frames have a "size"
       * (i.e. return address) pointing into the middle of the frame.
       */
/*       for (unsigned int j = 0; j < offsets[0]; ++j) */
/*         assert (offsets[j + 1] < layout->numBytes); */
    }
  }
  /* Generational */
  if (s->mutatorMarksCards) {
    assert (s->generational.cardMap == 
            &(s->generational.cardMapAbsolute
              [pointerToCardIndex(s->heap.start)]));
    assert (&(s->generational.cardMapAbsolute
              [pointerToCardIndex(s->heap.start + s->heap.size - 1)])
            < (s->generational.cardMap 
               + (s->generational.cardMapLength * CARD_MAP_ELEM_SIZE)));
  }
  assert (isAligned (s->heap.size, s->pageSize));
  assert (isAligned ((size_t)s->heap.start, CARD_SIZE));
  assert (isAlignedFrontier (s, s->heap.start + s->heap.oldGenSize));
  assert (isAlignedFrontier (s, s->heap.nursery));
  assert (isAlignedFrontier (s, s->frontier));
  assert (s->heap.nursery <= s->frontier);
  unless (0 == s->heap.size) {
    assert (s->heap.nursery <= s->frontier);
    assert (s->frontier <= s->limitPlusSlop);
    assert (s->limit == s->limitPlusSlop - LIMIT_SLOP);
/*     assert (hasBytesFree (s, 0, 0)); */
  }
  assert (s->secondaryHeap.start == NULL or s->heap.size == s->secondaryHeap.size);
/*   /\* Check that all pointers are into from space. *\/ */
/*   foreachGlobal (s, assertIsInFromSpace); */
/*   back = s->heap.start + s->oldGenSize; */
/*   if (DEBUG_DETAILED) */
/*     fprintf (stderr, "Checking old generation.\n"); */
/*   foreachPointerInRange (s, alignFrontier (s, s->heap.start), &back, FALSE, */
/*                          assertIsInFromSpace); */
/*   if (DEBUG_DETAILED) */
/*     fprintf (stderr, "Checking nursery.\n"); */
/*   foreachPointerInRange (s, s->nursery, &s->frontier, FALSE, */
/*                          assertIsInFromSpace); */
/*   /\* Current thread. *\/ */
/*   stack = s->currentThread->stack; */
/*   assert (isAlignedReserved (s, stack->reserved)); */
/*   assert (s->stackBottom == stackBottom (s, stack)); */
/*   assert (s->stackTop == stackTop (s, stack)); */
/*   assert (s->stackLimit == stackLimit (s, stack)); */
/*   assert (stack->used == currentStackUsed (s)); */
/*   assert (stack->used <= stack->reserved); */
/*   assert (s->stackBottom <= s->stackTop); */
  if (DEBUG)
    fprintf (stderr, "invariant passed\n");
  return TRUE;
}

static bool mutatorInvariant (GC_state s, bool frontier, bool stack) {
#if FALSE
  if (DEBUG)
    GC_display (s, stderr);
  if (frontier)
    assert (mutatorFrontierInvariant(s));
  if (stack)
    assert (mutatorStackInvariant(s));
#endif
  assert (invariant (s));
  return TRUE;
}

#endif /* #if ASSERT */
