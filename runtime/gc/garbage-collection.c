/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static void minorGC (GC_state s) {
  minorCheneyCopyGC (s);
}

static void majorGC (GC_state s, size_t bytesRequested, bool mayResize) {
  uintmax_t numGCs;
  size_t desiredSize;

  s->cumulativeStatistics.numMinorGCs = 0;
  numGCs = 
    s->cumulativeStatistics.numCopyingGCs 
    + s->cumulativeStatistics.numMarkCompactGCs;
  if (0 < numGCs
      and ((float)(s->cumulativeStatistics.numHashConsGCs) / (float)(numGCs)
           < s->ratios.hashCons))
    s->hashConsDuringGC = TRUE;
  desiredSize = 
    heapDesiredSize (s, s->lastMajorStatistics.bytesLive + bytesRequested, 0);
  if ((not FORCE_MARK_COMPACT)
      and not s->hashConsDuringGC // only markCompact can hash cons
      and s->heap.size < s->sysvals.ram
      and (not heapIsInit (&s->secondaryHeap)
           or secondaryHeapCreate (s, desiredSize)))
    majorCheneyCopyGC (s);
  else
    majorMarkCompactGC (s);
  s->hashConsDuringGC = FALSE;
  s->lastMajorStatistics.bytesLive = s->heap.oldGenSize;
  if (s->lastMajorStatistics.bytesLive > s->cumulativeStatistics.maxBytesLive)
    s->cumulativeStatistics.maxBytesLive = s->lastMajorStatistics.bytesLive;
  /* Notice that the s->bytesLive below is different than the s->bytesLive
   * used as an argument to heapAllocateSecondSemi above.  Above, it was 
   * an estimate.  Here, it is exactly how much was live after the GC.
   */
  if (mayResize)
    heapResize (s, s->lastMajorStatistics.bytesLive + bytesRequested);
  secondaryHeapResize (s);
  assert (s->heap.oldGenSize + bytesRequested <= s->heap.size);
}

static inline void enterGC (GC_state s) {
  if (s->profilingInfo.isOn) {
    /* We don't need to profileEnter for count profiling because it
     * has already bumped the counter.  If we did allow the bump, then
     * the count would look like function(s) had run an extra time.
     */  
    if (s->profilingInfo.stack
        and not (PROFILE_COUNT == s->profilingInfo.kind))
      GC_profileEnter (s);
  }
  s->amInGC = TRUE;
}

static inline void leaveGC (GC_state s) {
  if (s->profilingInfo.isOn) {
    if (s->profilingInfo.stack
        and not (PROFILE_COUNT == s->profilingInfo.kind))
      GC_profileLeave (s);
  }
  s->amInGC = FALSE;
}

static inline bool needGCTime (GC_state s) {
  return 
    DEBUG 
    or s->controls.summary 
    or s->controls.messages 
    or s->rusageIsEnabled;
}

static void doGC (GC_state s, 
                  size_t oldGenBytesRequested,
                  size_t nurseryBytesRequested, 
                  bool forceMajor,
                  bool mayResize) {
  uintmax_t gcTime;
  bool stackTopOk;
  size_t stackBytesRequested;
  struct rusage ru_start;
  size_t totalBytesRequested;
        
  enterGC (s);
  if (DEBUG or s->controls.messages)
    fprintf (stderr, "Starting gc.  Request %zu nursery bytes and %zu old gen bytes.\n",
             /*uintToCommaString*/(nurseryBytesRequested),
             /*uintToCommaString*/(oldGenBytesRequested));
  assert (invariant (s));
  if (needGCTime (s))
    startTiming (&ru_start);
  minorGC (s);
  stackTopOk = mutatorStackInvariant (s);
  stackBytesRequested = 
    stackTopOk ? 0 : stackSizeTotalAligned (s, stackGrowSize (s));
  totalBytesRequested = 
    oldGenBytesRequested 
    + nurseryBytesRequested
    + stackBytesRequested;
  if (forceMajor 
      or totalBytesRequested > s->heap.size - s->heap.oldGenSize)
    majorGC (s, totalBytesRequested, mayResize);
  heapSetNursery (s, oldGenBytesRequested + stackBytesRequested, 
                  nurseryBytesRequested);
  assert (heapHasBytesFree (s, oldGenBytesRequested + stackBytesRequested,
                            nurseryBytesRequested));
  unless (stackTopOk)
    stackGrow (s);
  setCurrentStack (s);
  if (needGCTime (s)) {
    gcTime = stopTiming (&ru_start, &s->cumulativeStatistics.ru_gc);
    s->cumulativeStatistics.maxPause = 
      max (s->cumulativeStatistics.maxPause, gcTime);
  } else
    gcTime = 0;  /* Assign gcTime to quell gcc warning. */
  if (DEBUG or s->controls.messages) {
    fprintf (stderr, "Finished gc.\n");
    fprintf (stderr, "time: %"PRIuMAX" ms\n", /*intToCommaString*/(gcTime));
    fprintf (stderr, "old gen size: %zu bytes (%.1f%%)\n", 
             /*intToCommaString*/(s->heap.oldGenSize),
             100.0 * ((double)(s->heap.oldGenSize) 
                      / (double)(s->heap.size)));
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
  assert (heapHasBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
  assert (invariant (s));
  leaveGC (s);
}

static inline void ensureMutatorInvariant (GC_state s, bool force) {
  if (force
      or not (mutatorFrontierInvariant(s))
      or not (mutatorStackInvariant(s))) {
    /* This GC will grow the stack, if necessary. */
    doGC (s, 0, currentThread(s)->bytesNeeded, force, TRUE);
  }
  assert (mutatorFrontierInvariant(s));
  assert (mutatorStackInvariant(s));
}

/* ensureFree (s, b) ensures that upon return
 *      b <= s->limitPlusSlop - s->frontier
 */
static inline void ensureFree (GC_state s, size_t bytesRequested) {
  assert (s->frontier <= s->limitPlusSlop);
  if (bytesRequested > (size_t)(s->limitPlusSlop - s->frontier))
    doGC (s, 0, bytesRequested, FALSE, TRUE);
  assert (bytesRequested <= (size_t)(s->limitPlusSlop - s->frontier));
}

static void switchToThread (GC_state s, objptr op) {
  if (DEBUG_THREADS) {
    GC_thread thread;
    GC_stack stack;
    
    thread = (GC_thread)(objptrToPointer (op, s->heap.start));
    stack = (GC_stack)(objptrToPointer (thread->stack, s->heap.start));
    
    fprintf (stderr, "switchToThread ("FMTOBJPTR")  used = %zu  reserved = %zu\n",
             op, stack->used, stack->reserved);
  }
  s->currentThread = op;
  setCurrentStack (s);
}

/* GC_startHandler does not do an enter()/leave(), even though it is
 * exported.  The basis library uses it via _import, not _prim, and so
 * does not treat it as a runtime call -- so the invariant in enter
 * would fail miserably.  It is OK because GC_startHandler must be
 * called from within a critical section.
 *
 * Don't make it inline, because it is also called in basis/Thread.c,
 * and when compiling with COMPILE_FAST, they may appear out of order.
 */
void GC_startHandler (GC_state s) {
  /* Switch to the signal handler thread. */
  if (DEBUG_SIGNALS) {
    fprintf (stderr, "GC_startHandler\n");
  }
  assert (s->atomicState == 1);
  assert (s->signalsInfo.signalIsPending);
  s->signalsInfo.signalIsPending = FALSE;
  s->signalsInfo.amInSignalHandler = TRUE;
  s->savedThread = s->currentThread;
  /* Set s->atomicState to 2 when switching to the signal handler
   * thread; leaving the runtime will decrement s->atomicState to 1,
   * the signal handler will then run atomically and will finish by
   * switching to the thread to continue with, which will decrement
   * s->atomicState to 0.
   */
  s->atomicState = 2;
}

void GC_finishHandler (GC_state s) {
  if (DEBUG_SIGNALS)
    fprintf (stderr, "GC_finishHandler ()\n");
  assert (s->atomicState == 1);
  s->signalsInfo.amInSignalHandler = FALSE;     
}

static inline void maybeSwitchToHandler (GC_state s) {
  if (s->atomicState == 1 
      and s->signalsInfo.signalIsPending) {
    GC_startHandler (s);
    switchToThread (s, s->signalHandlerThread);
  }
}

/* void GC_switchToThread (GC_state s, GC_thread t, uint ensureBytesFree) { */
/*         if (DEBUG_THREADS) */
/*                 fprintf (stderr, "GC_switchToThread (0x%08x, %u)\n", (uint)t, ensureBytesFree); */
/*         if (FALSE) { */
/*                 /\* This branch is slower than the else branch, especially  */
/*                  * when debugging is turned on, because it does an invariant */
/*                  * check on every thread switch. */
/*                  * So, we'll stick with the else branch for now. */
/*                  *\/ */
/*                 enter (s); */
/*                 s->currentThread->bytesNeeded = ensureBytesFree; */
/*                 switchToThread (s, t); */
/*                 s->canHandle--; */
/*                 maybeSwitchToHandler (s); */
/*                 ensureMutatorInvariant (s, FALSE); */
/*                 assert (mutatorFrontierInvariant(s)); */
/*                 assert (mutatorStackInvariant(s)); */
/*                 leave (s); */
/*         } else { */
/*                 /\* BEGIN: enter(s); *\/ */
/*                 s->currentThread->stack->used = currentStackUsed (s); */
/*                 s->currentThread->exnStack = s->exnStack; */
/*                 atomicBegin (s); */
/*                 /\* END: enter(s); *\/ */
/*                 s->currentThread->bytesNeeded = ensureBytesFree; */
/*                 switchToThread (s, t); */
/*                 s->canHandle--; */
/*                 maybeSwitchToHandler (s); */
/*                 /\* BEGIN: ensureMutatorInvariant *\/ */
/*                 if (not (mutatorFrontierInvariant(s)) */
/*                         or not (mutatorStackInvariant(s))) { */
/*                         /\* This GC will grow the stack, if necessary. *\/ */
/*                         doGC (s, 0, s->currentThread->bytesNeeded, FALSE, TRUE); */
/*                 }  */
/*                 /\* END: ensureMutatorInvariant *\/ */
/*                 /\* BEGIN: leave(s); *\/ */
/*                 atomicEnd (s); */
/*                 /\* END: leave(s); *\/ */
/*         } */
/*         assert (mutatorFrontierInvariant(s)); */
/*         assert (mutatorStackInvariant(s)); */
/* } */

void GC_gc (GC_state s, size_t bytesRequested, bool force,
            char *file, int line) {
  if (DEBUG or s->controls.messages)
    fprintf (stderr, "%s %d: GC_gc\n", file, line);
  enter (s);
  /* When the mutator requests zero bytes, it may actually need as
   * much as LIMIT_SLOP.
   */
  if (0 == bytesRequested)
    bytesRequested = LIMIT_SLOP;
  currentThread(s)->bytesNeeded = bytesRequested;
  maybeSwitchToHandler (s);
  ensureMutatorInvariant (s, force);
  assert (mutatorFrontierInvariant(s));
  assert (mutatorStackInvariant(s));
  leave (s);
}
