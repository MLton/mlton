/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if ASSERT

static inline void assertObjptrIsInFromSpace (GC_state s, objptr *opp) {
  unless (objptrIsInFromSpace (s, *opp))
    die ("gc.c: assertObjptrIsInFromSpace "
         "opp = "FMTPTR"  "
         "*opp = "FMTOBJPTR"\n",
         (uintptr_t)opp, *opp);
}

static bool invariant (GC_state s) {
  if (DEBUG)
    fprintf (stderr, "invariant\n");
  assert (ratiosOk (s->ratios));
  /* Frame layouts */
  for (unsigned int i = 0; i < s->frameLayoutsLength; ++i) {
    GC_frameLayout *layout;
    
    layout = &(s->frameLayouts[i]);
    if (layout->size > 0) {
      GC_frameOffsets offsets;
      
      assert (layout->size <= s->maxFrameSize);
      offsets = layout->offsets;
    }
  }
  /* Generational */
  if (s->mutatorMarksCards) {
    assert (s->generationalMaps.cardMap == 
            &(s->generationalMaps.cardMapAbsolute
              [pointerToCardIndex(s->heap.start)]));
    assert (&(s->generationalMaps.cardMapAbsolute
              [pointerToCardIndex(s->heap.start + s->heap.size - 1)])
            < (s->generationalMaps.cardMap 
               + (s->generationalMaps.cardMapLength * CARD_MAP_ELEM_SIZE)));
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
    assert (heapHasBytesFree (s, 0, 0));
  }
  assert (s->secondaryHeap.start == NULL 
          or s->heap.size == s->secondaryHeap.size);
  /* Check that all pointers are into from space. */
  foreachGlobalObjptr (s, assertObjptrIsInFromSpace);
  pointer back = s->heap.start + s->heap.oldGenSize;
  if (DEBUG_DETAILED)
    fprintf (stderr, "Checking old generation.\n");
  foreachObjptrInRange (s, alignFrontier (s, s->heap.start), &back, 
                        FALSE, assertObjptrIsInFromSpace);
  if (DEBUG_DETAILED)
    fprintf (stderr, "Checking nursery.\n");
  foreachObjptrInRange (s, s->heap.nursery, &s->frontier, 
                        FALSE, assertObjptrIsInFromSpace);
  /* Current thread. */
  GC_stack stack = currentThreadStack(s);
  assert (isAlignedReserved (s, stack->reserved));
  assert (s->stackBottom == stackBottom (s, stack));
  assert (s->stackTop == stackTop (s, stack));
  assert (s->stackLimit == stackLimit (s, stack));
  assert (s->stackBottom <= s->stackTop);
  assert (stack->used == currentStackUsed (s));
  assert (stack->used <= stack->reserved);
  if (DEBUG)
    fprintf (stderr, "invariant passed\n");
  return TRUE;
}

static bool mutatorFrontierInvariant (GC_state s) {
  GC_thread ct = currentThread(s);
  return (ct->bytesNeeded <= s->limitPlusSlop - s->frontier);
}

static bool mutatorStackInvariant (GC_state s) {
  GC_stack sk = currentThreadStack(s);
  return (stackTop (s, sk) <= stackLimit (s, sk) + topFrameSize (s, sk));
}

static bool mutatorInvariant (GC_state s, bool frontier, bool stack) {
  if (DEBUG)
    displayGCState (s, stderr);
  if (frontier)
    assert (mutatorFrontierInvariant(s));
  if (stack)
    assert (mutatorStackInvariant(s));
  assert (invariant (s));
  return TRUE;
}

#endif /* #if ASSERT */
