/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void minorGC (GC_state s) {
  minorCheneyCopyGC (s);
}

void majorGC (GC_state s, size_t bytesRequested, bool mayResize) {
  uintmax_t numGCs;
  size_t desiredSize;

  s->lastMajorStatistics->numMinorGCs = 0;
  numGCs = 
    s->cumulativeStatistics->numCopyingGCs 
    + s->cumulativeStatistics->numMarkCompactGCs;
  if (0 < numGCs
      and ((float)(s->cumulativeStatistics->numHashConsGCs) / (float)(numGCs)
           < s->controls->ratios.hashCons))
    s->hashConsDuringGC = TRUE;
  desiredSize = 
    sizeofHeapDesired (s, s->lastMajorStatistics->bytesLive + bytesRequested, 0);
  if (not FORCE_MARK_COMPACT
      and not s->hashConsDuringGC // only markCompact can hash cons
      and s->heap->size < s->sysvals.ram
      and (not isHeapInit (s->secondaryHeap)
           or createHeapSecondary (s, desiredSize)))
    majorCheneyCopyGC (s);
  else
    majorMarkCompactGC (s);
  s->hashConsDuringGC = FALSE;
  s->lastMajorStatistics->bytesLive = s->heap->oldGenSize;
  if (s->lastMajorStatistics->bytesLive > s->cumulativeStatistics->maxBytesLive)
    s->cumulativeStatistics->maxBytesLive = s->lastMajorStatistics->bytesLive;
  if (s->lastMajorStatistics->bytesLive > s->cumulativeStatistics->maxBytesLiveSinceReset)
    s->cumulativeStatistics->maxBytesLiveSinceReset = s->lastMajorStatistics->bytesLive;
  /* Notice that the s->bytesLive below is different than the
   * s->bytesLive used as an argument to createHeapSecondary above.
   * Above, it was an estimate.  Here, it is exactly how much was live
   * after the GC.
   */
  if (mayResize)
    resizeHeap (s, s->lastMajorStatistics->bytesLive + bytesRequested);
  resizeHeapSecondary (s);
  assert (s->heap->oldGenSize + bytesRequested <= s->heap->size);
}

void growStackCurrent (GC_state s, bool allocInOldGen) {
  size_t size;
  GC_stack stack;

  size = sizeofStackGrow (s, getStackCurrent(s));
  if (DEBUG_STACKS or s->controls->messages)
    fprintf (stderr, "[GC: Growing stack to size %s bytes.]\n",
             uintmaxToCommaString(sizeofStackWithHeaderAligned (s, size)));
  assert (allocInOldGen ? 
          hasHeapBytesFree (s, sizeofStackWithHeaderAligned (s, size), 0) :
          hasHeapBytesFree (s, 0, sizeofStackWithHeaderAligned (s, size)));
  stack = newStack (s, size, allocInOldGen);
  copyStack (s, getStackCurrent(s), stack);
  getThreadCurrent(s)->stack = pointerToObjptr ((pointer)stack, s->heap->start);
  markCard (s, objptrToPointer (getThreadCurrentObjptr(s), s->heap->start));
}

void enterGC (GC_state s) {
  if (s->profiling.isOn) {
    /* We don't need to profileEnter for count profiling because it
     * has already bumped the counter.  If we did allow the bump, then
     * the count would look like function(s) had run an extra time.
     */  
    if (s->profiling.stack
        and not (PROFILE_COUNT == s->profiling.kind))
      GC_profileEnter (s);
  }
  s->amInGC = TRUE;
}

void leaveGC (GC_state s) {
  if (s->profiling.isOn) {
    if (s->profiling.stack
        and not (PROFILE_COUNT == s->profiling.kind))
      GC_profileLeave (s);
  }
  s->amInGC = FALSE;
}

void performGC (GC_state s, 
                size_t oldGenBytesRequested,
                size_t nurseryBytesRequested, 
                bool forceMajor,
                bool mayResize) {
  uintmax_t gcTime;
  bool stackTopOk;
  size_t stackBytesRequested;
  struct timeval tv_start;
  size_t totalBytesRequested;

  enterGC (s);

  if (DEBUG or s->controls->messages)
    fprintf (stderr, "[GC: Starting gc; request %s nursery bytes and %s old-gen bytes.]\n",
             uintmaxToCommaString(nurseryBytesRequested),
             uintmaxToCommaString(oldGenBytesRequested));
  assert (invariantForGC (s));
  if (needGCTime (s))
    startWallTiming (&tv_start);
  minorGC (s);
  stackTopOk = invariantForMutatorStack (s);
  stackBytesRequested = 
    stackTopOk 
    ? 0 
    : sizeofStackWithHeaderAligned (s, sizeofStackGrow (s, getStackCurrent (s)));
  totalBytesRequested = 
    oldGenBytesRequested 
    + stackBytesRequested;
  getThreadCurrent(s)->bytesNeeded = nurseryBytesRequested;
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    /* It could be that other threads have already worked to satisfy their own
       requests.  We need to make sure that we don't invalidate the work
       they've done.
    */
    if (getThreadCurrent(&s->procStates[proc])->bytesNeeded == 0) {
      getThreadCurrent(&s->procStates[proc])->bytesNeeded = GC_HEAP_LIMIT_SLOP;
    }
    totalBytesRequested += getThreadCurrent(&s->procStates[proc])->bytesNeeded;
    totalBytesRequested += GC_BONUS_SLOP;
  }

  if (forceMajor 
      or totalBytesRequested > s->heap->availableSize - s->heap->oldGenSize)
    majorGC (s, totalBytesRequested, mayResize);
  setGCStateCurrentHeap (s, oldGenBytesRequested + stackBytesRequested, 
                         nurseryBytesRequested, false);
  assert (hasHeapBytesFree (s, oldGenBytesRequested + stackBytesRequested,
                            nurseryBytesRequested));
  unless (stackTopOk)
    growStackCurrent (s, TRUE);
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    /* DOC XXX must come first to setup maps properly */
    s->procStates[proc].generationalMaps = s->generationalMaps;
    setGCStateCurrentThreadAndStack (&s->procStates[proc]);
  }
  if (needGCTime (s)) {
    gcTime = stopWallTiming (&tv_start, &s->cumulativeStatistics->tv_gc);
    s->cumulativeStatistics->maxPause = 
      max (s->cumulativeStatistics->maxPause, gcTime);
  } else
    gcTime = 0;  /* Assign gcTime to quell gcc warning. */
  if (DEBUG or s->controls->messages) {
    fprintf (stderr, "[GC: Finished gc; time: %s ms, old-gen: %s bytes (%.1f%%).]\n",
             uintmaxToCommaString(gcTime),
             uintmaxToCommaString(s->heap->oldGenSize),
             100.0 * ((double)(s->heap->oldGenSize) 
                      / (double)(s->heap->availableSize)));
  }
  /* Send a GC signal. */
  if (s->signalsInfo.gcSignalHandled
      and s->signalHandlerThread != BOGUS_OBJPTR) {
    if (DEBUG_SIGNALS)
      fprintf (stderr, "GC Signal pending.\n");
    s->signalsInfo.gcSignalPending = TRUE;
    unless (s->signalsInfo.amInSignalHandler) 
      s->signalsInfo.signalIsPending = TRUE;
  }
  if (DEBUG) 
    displayGCState (s, stderr);
  assert (hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
  assert (invariantForGC (s));
  leaveGC (s);
}

size_t fillGap (__attribute__ ((unused)) GC_state s, pointer start, pointer end) {
  size_t diff = end - start;

  if (diff == 0) {
    return 0;
  }

  if (DEBUG)
    fprintf (stderr, "[GC: Filling gap between "FMTPTR" and "FMTPTR" (size = %zu).]\n", 
             (uintptr_t)start, (uintptr_t)end, diff);

  if (start) {
    /* See note in the array case of foreach.c (line 103) */
    if (diff >= GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) {
      assert (diff >= GC_ARRAY_HEADER_SIZE);     
      /* Counter */
      *((GC_arrayCounter *)start) = 0;
      start = start + GC_ARRAY_COUNTER_SIZE;
      /* Length */
      *((GC_arrayLength *)start) = diff - GC_ARRAY_HEADER_SIZE;
      start = start + GC_ARRAY_LENGTH_SIZE;
      /* Header */
      *((GC_header *)start) = GC_WORD8_VECTOR_HEADER;
      start = start + GC_HEADER_SIZE;
    }
    else if (diff == GC_HEADER_SIZE) {
      *((GC_header *)start) = GC_HEADER_ONLY_HEADER;
      start = start + GC_HEADER_SIZE;      
    }
    else if (diff >= GC_BONUS_SLOP) {
      assert (diff < INT_MAX);
      *((GC_header *)start) = GC_FILL_HEADER;
      start = start + GC_HEADER_SIZE;
      *((GC_smallGapSize *)start) = diff - (GC_HEADER_SIZE + GC_SMALL_GAP_SIZE_SIZE);
      start = start + GC_SMALL_GAP_SIZE_SIZE;
    }
    else {
      assert(0 == diff);
      /* XXX */
      fprintf (stderr, "FOUND A GAP OF %zu BYTES!\n", diff);
      exit (1);
    }

    /* XXX debug only */
    /*
    while (start < end) {
      *(start++) = 0xDF;
    }
    */

    return diff;
  }  
  else {
    return 0;
  }
}

static void maybeSatisfyAllocationRequestLocally (GC_state s,
                                                  size_t nurseryBytesRequested) {
  /* First try and take another chunk from the shared nursery */
  while (TRUE)
  {
    /* This is the only read of the global frontier -- never read it again
       until after the swap. */
    pointer oldFrontier = s->heap->frontier;
    pointer newHeapFrontier, newProcFrontier;
    pointer newStart;
    /* heap->start and heap->size are read-only (unless you hold the global
       lock) so it's ok to read them here */
    size_t availableBytes = (size_t)((s->heap->start + s->heap->availableSize)
                                     - oldFrontier);

    /* If another thread is trying to get exclusive access, the join the
       queue. */
    if (Proc_threadInSection (s)) {
      if (DEBUG)
        fprintf (stderr, "[GC: aborting local alloc: mutex.]\n");
      return;
    }
    /* See if the mutator frontier invariant is already true */
    assert (s->limitPlusSlop >= s->frontier);
    if (nurseryBytesRequested <= (size_t)(s->limitPlusSlop - s->frontier)) {
      if (DEBUG)
        fprintf (stderr, "[GC: aborting local alloc: satisfied.]\n");
      return;
    }
    /* Perhaps there is not enough space in the nursery to satify this
       request; if that's true then we need to do a full collection */
    if (nurseryBytesRequested + GC_BONUS_SLOP > availableBytes) {
      if (DEBUG)
        fprintf (stderr, "[GC: aborting local alloc: no space.]\n");
      return;
    }

    /* OK! We might possibly satisfy this request without the runtime lock!
       Let's see what that will entail... */
       
    /* Now see if we were the most recent thread to allocate */
    if (oldFrontier == s->limitPlusSlop + GC_BONUS_SLOP) {
      /* This is the next chunk so no need to fill */
      newHeapFrontier = s->frontier + nurseryBytesRequested + GC_BONUS_SLOP;
      /* Leave "start" and "frontier" where they are */
      newStart = s->start;
      newProcFrontier = s->frontier;
    }
    else {               
      /* Fill the old gap */
      fillGap (s, s->frontier, s->limitPlusSlop + GC_BONUS_SLOP);
      /* Don't update frontier or limitPlusSlop since we will either
         overwrite them (if we succeed) or just fill the same gap again
         (if we fail).  (There is no obvious other pair of values that
         we can set them to that is safe.) */
      newHeapFrontier = oldFrontier + nurseryBytesRequested + GC_BONUS_SLOP;
      newProcFrontier = oldFrontier;
      /* Move "start" since the space between old-start and frontier is not
         necessary filled */
      newStart = oldFrontier;
    }
    
    if (__sync_bool_compare_and_swap (&s->heap->frontier, 
                                      oldFrontier, newHeapFrontier)) {
      if (DEBUG)
        fprintf (stderr, "[GC: Local alloction of chunk @ "FMTPTR".]\n", 
                 (uintptr_t)newProcFrontier);

      s->start = newStart;
      s->frontier = newProcFrontier;
      assert (isFrontierAligned (s, s->frontier));
      s->limitPlusSlop = newHeapFrontier - GC_BONUS_SLOP;
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

      return;
    }
    else {
      if (DEBUG)
        fprintf (stderr, "[GC: Contention for alloction (frontier is "FMTPTR").]\n", 
                 (uintptr_t)s->heap->frontier);
    }
  }
}

// assumes that stack->used and thread->exnstack are up to date
// assumes exclusive access to runtime if !mustEnter
// forceGC = force major collection
void ensureHasHeapBytesFreeAndOrInvariantForMutator (GC_state s, bool forceGC,
                                                     bool ensureFrontier,
                                                     bool ensureStack,
                                                     size_t oldGenBytesRequested,
                                                     size_t nurseryBytesRequested) {
  bool stackTopOk;
  size_t stackBytesRequested;

  /* To ensure the mutator frontier invariant, set the requested bytes
     to include those needed by the thread.
   */
  if (ensureFrontier) {
    nurseryBytesRequested += getThreadCurrent(s)->bytesNeeded;
  }

  /* XXX (sort of) copied from performGC */
  stackTopOk = (not ensureStack) or invariantForMutatorStack (s);
  stackBytesRequested =
    stackTopOk 
    ? 0 
    : sizeofStackWithHeaderAligned (s, sizeofStackGrow (s, getStackCurrent (s)));

  /* try to satisfy (at least part of the) request locally */
  maybeSatisfyAllocationRequestLocally (s, nurseryBytesRequested + stackBytesRequested);

  if (not stackTopOk
      and (hasHeapBytesFree (s, 0, stackBytesRequested))) {
    if (DEBUG or s->controls->messages)
      fprintf (stderr, "GC: growing stack locally... [%d]\n",
               s->procStates ? Proc_processorNumber (s) : -1);
    growStackCurrent (s, FALSE);
    setGCStateCurrentThreadAndStack (s);
  }

  if (DEBUG or s->controls->messages) {
    fprintf (stderr, "GC: stackInvariant: %d,%d hasHeapBytesFree: %d inSection: %d force: %d [%d]\n", 
             ensureStack, ensureStack and invariantForMutatorStack (s),
             hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested),
             Proc_threadInSection (s),
             forceGC,
             s->procStates ? Proc_processorNumber (s) : -1);
  }

  if (/* check the stack of the current thread */
      ((ensureStack and not invariantForMutatorStack (s))
          and (s->syncReason = SYNC_STACK))
      /* this subsumes invariantForMutatorFrontier */
      or (not hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested)
            and (s->syncReason = SYNC_HEAP))
      /* another thread is waiting for exclusive access */
      or Proc_threadInSection (s)
      /* we are forcing a major collection */
      or (forceGC
           and (s->syncReason = SYNC_FORCE))) {
    /* Copy the value here so other threads will see it (if we synchronize and
       one of the other threads does the work). */
    if (isObjptr (getThreadCurrentObjptr(s)))
      getThreadCurrent(s)->bytesNeeded = nurseryBytesRequested;

    ENTER0 (s);
    /* XXX should this go here? */
    switchToSignalHandlerThreadIfNonAtomicAndSignalPending (s);

    /* Recheck invariants now that we hold the lock */
    if ((ensureStack and not invariantForMutatorStack (s))
        or not hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested)
        or forceGC) {
      performGC (s, oldGenBytesRequested, nurseryBytesRequested, forceGC, TRUE);
    }
    else
      if (DEBUG or s->controls->messages)
        fprintf (stderr, "GC: Skipping GC (inside of sync). [%d]\n", s->procStates ? Proc_processorNumber (s) : -1);

    LEAVE0 (s);
  }
  else {
    if (DEBUG or s->controls->messages)
      fprintf (stderr, "GC: Skipping GC (invariants already hold / request satisfied locally). [%d]\n", s->procStates ? Proc_processorNumber (s) : -1);

    /* These are safe even without ENTER/LEAVE */
    assert (isAligned (s->heap->size, s->sysvals.pageSize));
    assert (isAligned ((size_t)s->heap->start, CARD_SIZE));
    assert (isFrontierAligned (s, s->heap->start + s->heap->oldGenSize));
    assert (isFrontierAligned (s, s->heap->nursery));
    assert (isFrontierAligned (s, s->frontier));
    assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
    assert (s->heap->nursery <= s->heap->start + s->heap->availableSize);
    assert (s->heap->nursery <= s->frontier or 0 == s->frontier);
    assert (s->start <= s->frontier);
    unless (0 == s->heap->size or 0 == s->frontier) {
      assert (s->frontier <= s->limitPlusSlop);
      assert (s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP);
      assert (hasHeapBytesFree (s, 0, 0));
    }
  }
  assert (not ensureFrontier or invariantForMutatorFrontier(s));
  assert (not ensureStack or invariantForMutatorStack(s));
}

void GC_collect (GC_state s, size_t bytesRequested, bool force,
                 char *file, int line) {
  
  if (DEBUG or s->controls->messages)
    fprintf (stderr, "%s %d: GC_collect [%d]\n", file, line,
             Proc_processorNumber (s));

  /* When the mutator requests zero bytes, it may actually need as
   * much as GC_HEAP_LIMIT_SLOP.
   */
  if (0 == bytesRequested)
    bytesRequested = s->controls->allocChunkSize;
  else if (bytesRequested < s->controls->allocChunkSize)
    bytesRequested = s->controls->allocChunkSize;
  else
    bytesRequested += GC_HEAP_LIMIT_SLOP;

  /* XXX copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  getThreadCurrent(s)->bytesNeeded = bytesRequested;

  ensureHasHeapBytesFreeAndOrInvariantForMutator (s, force, 
                                                  TRUE, TRUE,
                                                  0, 0);
}

uint32_t FFI_getOp (GC_state s) {
  return s->ffiOp;
}
