/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#define SOURCES_INDEX_UNKNOWN 0
#define SOURCES_INDEX_GC      1
#define SOURCE_SEQ_GC         1
#define SOURCE_SEQ_UNKNOWN    0

static uint32_t numStackFrames;
static uint32_t *callStack;

static void fillCallStack (__attribute__ ((unused))GC_state s, 
                           GC_frameIndex i) {
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "fillCallStack ("FMTFI")\n", i);
  callStack[numStackFrames] = i;
  numStackFrames++;
}

void GC_callStack (GC_state s, pointer p) {
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "GC_callStack\n");
  numStackFrames = 0;
  callStack = (uint32_t*)p;
  foreachStackFrame (s, fillCallStack);
}

static void incNumStackFrames (__attribute__ ((unused)) GC_state s, 
                               __attribute__ ((unused)) GC_frameIndex i) {
  numStackFrames++;
}

uint32_t GC_numStackFrames (GC_state s) {
  numStackFrames = 0;
  foreachStackFrame (s, incNumStackFrames);
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "%"PRIu32" = GC_numStackFrames\n", numStackFrames);
  return numStackFrames;
}

static inline uint32_t topFrameSourceSeqIndex (GC_state s, GC_stack stack) {
  return s->profiling.frameSources[topFrameIndex (s, stack)];
}

uint32_t* GC_frameIndexSourceSeq (GC_state s, GC_frameIndex frameIndex) {
  uint32_t *res;

  res = s->profiling.sourceSeqs[s->profiling.frameSources[frameIndex]];
  if (DEBUG_CALL_STACK)
    fprintf (stderr, FMTPTR" = GC_frameIndexSourceSeq ("FMTFI")\n",
             (uintptr_t)res, frameIndex);
  return res;
}

inline char* GC_sourceName (GC_state s, uint32_t i) {
  if (i < s->profiling.sourcesLength)
    return s->profiling.sourceNames[s->profiling.sources[i].nameIndex];
  else
    return s->profiling.sourceNames[i - s->profiling.sourcesLength];
}

static inline GC_profileStack profileStackInfo (GC_state s, uint32_t i) {
  assert (s->profiling.data != NULL);
  return &(s->profiling.data->stack[i]);
}

static inline uint32_t profileMaster (GC_state s, uint32_t i) {
  return s->profiling.sources[i].nameIndex + s->profiling.sourcesLength;
}

static inline void removeFromStack (GC_state s, uint32_t i) {
  GC_profileData p;
  GC_profileStack ps;
  uintmax_t totalInc;

  p = s->profiling.data;
  ps = profileStackInfo (s, i);
  totalInc = p->total - ps->lastTotal;
  if (DEBUG_PROFILE)
    fprintf (stderr, "removing %s from stack  ticksInc = %"PRIuMAX"  ticksInGCInc = %"PRIuMAX"\n",
             GC_sourceName (s, i), totalInc,
             p->totalGC - ps->lastTotalGC);
  ps->ticks += totalInc;
  ps->ticksInGC += p->totalGC - ps->lastTotalGC;
}

static void setProfTimer (long usec) {
  struct itimerval iv;
  
  iv.it_interval.tv_sec = 0;
  iv.it_interval.tv_usec = usec;
  iv.it_value.tv_sec = 0;
  iv.it_value.tv_usec = usec;
  unless (0 == setitimer (ITIMER_PROF, &iv, NULL))
    die ("setProfTimer failed");
}

void GC_profileDone (GC_state s) {
  GC_profileData p;
  uint32_t sourceIndex;

  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_profileDone ()\n");
  assert (s->profiling.isOn);
  if (PROFILE_TIME == s->profiling.kind)
    setProfTimer (0);
  s->profiling.isOn = FALSE;
  p = s->profiling.data;
  if (s->profiling.stack) {
    for (sourceIndex = 0;
         sourceIndex < s->profiling.sourcesLength + s->profiling.sourceNamesLength;
         sourceIndex++) {
      if (p->stack[sourceIndex].numOccurrences > 0) {
        if (DEBUG_PROFILE)
          fprintf (stderr, "done leaving %s\n",
                   GC_sourceName (s, sourceIndex));
        removeFromStack (s, sourceIndex);
      }
    }
  }
}

static int profileDepth = 0;

static void profileIndent (void) {
  int i;

  for (i = 0; i < profileDepth; ++i)
    fprintf (stderr, " ");
}

static inline void profileEnterSource (GC_state s, uint32_t i) {
  GC_profileData p;
  GC_profileStack ps;
  
  p = s->profiling.data;
  ps = profileStackInfo (s, i);
  if (0 == ps->numOccurrences) {
    ps->lastTotal = p->total;
    ps->lastTotalGC = p->totalGC;
  }
  ps->numOccurrences++;
}

static void profileEnter (GC_state s, uint32_t sourceSeqIndex) {
  uint32_t i;
  GC_profileData p;
  uint32_t sourceIndex;
  uint32_t *sourceSeq;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileEnter (%"PRIu32")\n", sourceSeqIndex);
  assert (s->profiling.stack);
  assert (sourceSeqIndex < s->profiling.sourceSeqsLength);
  p = s->profiling.data;
  sourceSeq = s->profiling.sourceSeqs[sourceSeqIndex];
  for (i = 1; i <= sourceSeq[0]; i++) {
    sourceIndex = sourceSeq[i];
    if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
      profileIndent ();
      fprintf (stderr, "(entering %s\n",
               GC_sourceName (s, sourceIndex));
      profileDepth++;
    }
    profileEnterSource (s, sourceIndex);
    profileEnterSource (s, profileMaster (s, sourceIndex));
  }
}

static void enterFrame (GC_state s, uint32_t i) {
  profileEnter (s, s->profiling.frameSources[i]);
}

static inline void profileLeaveSource (GC_state s, uint32_t i) {
  GC_profileData p;
  GC_profileStack ps;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileLeaveSource (%"PRIu32")\n", i);
  p = s->profiling.data;
  ps = profileStackInfo (s, i);
  assert (ps->numOccurrences > 0);
  ps->numOccurrences--;
  if (0 == ps->numOccurrences)
    removeFromStack (s, i);
}

static void profileLeave (GC_state s, uint32_t sourceSeqIndex) {
  int32_t i;
  GC_profileData p;
  uint32_t sourceIndex;
  uint32_t *sourceSeq;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileLeave (%"PRIu32")\n", sourceSeqIndex);
  assert (s->profiling.stack);
  assert (sourceSeqIndex < s->profiling.sourceSeqsLength);
  p = s->profiling.data;
  sourceSeq = s->profiling.sourceSeqs[sourceSeqIndex];
  for (i = sourceSeq[0]; i > 0; i--) {
    sourceIndex = sourceSeq[i];
    if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
      profileDepth--;
      profileIndent ();
      fprintf (stderr, "leaving %s)\n",
               GC_sourceName (s, sourceIndex));
    }
    profileLeaveSource (s, sourceIndex);
    profileLeaveSource (s, profileMaster (s, sourceIndex));
  }
}

static inline void profileInc (GC_state s, size_t amount, uint32_t sourceSeqIndex) {
  uint32_t *sourceSeq;
  uint32_t topSourceIndex;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileInc (%zu, %"PRIu32")\n",
             amount, sourceSeqIndex);
  assert (sourceSeqIndex < s->profiling.sourceSeqsLength);
  sourceSeq = s->profiling.sourceSeqs[sourceSeqIndex];
  topSourceIndex = sourceSeq[0] > 0 ? sourceSeq[sourceSeq[0]] : SOURCES_INDEX_UNKNOWN;
  if (DEBUG_PROFILE) {
    profileIndent ();
    fprintf (stderr, "bumping %s by %zu\n",
             GC_sourceName (s, topSourceIndex), amount);
  }
  s->profiling.data->countTop[topSourceIndex] += amount;
  s->profiling.data->countTop[profileMaster (s, topSourceIndex)] += amount;
  if (s->profiling.stack)
    profileEnter (s, sourceSeqIndex);
  if (SOURCES_INDEX_GC == topSourceIndex)
    s->profiling.data->totalGC += amount;
  else
    s->profiling.data->total += amount;
  if (s->profiling.stack)
    profileLeave (s, sourceSeqIndex);
}

void GC_profileEnter (GC_state s) {
  profileEnter (s, topFrameSourceSeqIndex (s, currentThreadStack (s)));
}

void GC_profileLeave (GC_state s) {
  profileLeave (s, topFrameSourceSeqIndex (s, currentThreadStack (s)));
}

void GC_profileInc (GC_state s, size_t amount) {
  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_profileInc (%zu)\n", amount);
  profileInc (s, amount,
              s->amInGC 
              ? SOURCE_SEQ_GC 
              : topFrameSourceSeqIndex (s, currentThreadStack (s)));
}

void GC_profileAllocInc (GC_state s, size_t amount) {
  if (s->profiling.isOn and (PROFILE_ALLOC == s->profiling.kind)) {
    if (DEBUG_PROFILE)
      fprintf (stderr, "GC_profileAllocInc (%u)\n", (uint)amount);
    GC_profileInc (s, amount);
  }
}

static void showProf (GC_state s) {
  uint32_t i;
  uint32_t j;
  
  fprintf (stdout, "0x%08"PRIx32"\n", s->magic);
  fprintf (stdout, "%"PRIu32"\n", s->profiling.sourceNamesLength);
  for (i = 0; i < s->profiling.sourceNamesLength; i++)
    fprintf (stdout, "%s\n", s->profiling.sourceNames[i]);
  fprintf (stdout, "%"PRIu32"\n", s->profiling.sourcesLength);
  for (i = 0; i < s->profiling.sourcesLength; i++)
    fprintf (stdout, "%"PRIu32" %"PRIu32"\n",
             s->profiling.sources[i].nameIndex,
             s->profiling.sources[i].successorsIndex);
  fprintf (stdout, "%"PRIu32"\n", s->profiling.sourceSeqsLength);
  for (i = 0; i < s->profiling.sourceSeqsLength; i++) {
    uint32_t *sourceSeq;
    
    sourceSeq = s->profiling.sourceSeqs[i];
    for (j = 1; j <= sourceSeq[0]; j++)
      fprintf (stdout, "%"PRIu32" ", sourceSeq[j]);
    fprintf (stdout, "\n");
  }
}

GC_profileData GC_profileNew (GC_state s) {
  GC_profileData p;
  uint32_t size;

  p = (GC_profileData)(malloc_safe (sizeof(*p)));
  p->total = 0;
  p->totalGC = 0;
  size = s->profiling.sourcesLength + s->profiling.sourceNamesLength;
  p->countTop = (uintmax_t*)(calloc_safe(size, sizeof(*(p->countTop))));
  if (s->profiling.stack)
    p->stack = 
      (struct GC_profileStack *)
      (calloc_safe(size, sizeof(*(p->stack))));
  if (DEBUG_PROFILE)
    fprintf (stderr, FMTPTR" = GC_profileNew ()\n", (uintptr_t)p);
  return p;
}

void GC_profileFree (GC_state s, GC_profileData p) {
  free (p->countTop);
  if (s->profiling.stack)
    free (p->stack);
  free (p);
}

static void profileWriteCount (GC_state s, GC_profileData p, int fd, uint32_t i) {
  writeUintmaxU (fd, p->countTop[i]);
  if (s->profiling.stack) {
    GC_profileStack ps;
    
    ps = &(p->stack[i]);
    writeString (fd, " ");
    writeUintmaxU (fd, ps->ticks);
    writeString (fd, " ");
    writeUintmaxU (fd, ps->ticksInGC);
  }
  writeNewline (fd);
}

void GC_profileWrite (GC_state s, GC_profileData p, int fd) {
  uint32_t i;
  char* kind;

  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_profileWrite\n");
  writeString (fd, "MLton prof\n");
  kind = "";
  switch (s->profiling.kind) {
  case PROFILE_ALLOC:
    kind = "alloc\n";
    break;
  case PROFILE_COUNT:
    kind = "count\n";
    break;
  case PROFILE_NONE:
    die ("impossible PROFILE_NONE");
    break;
  case PROFILE_TIME:
    kind = "time\n";
    break;
  }
  writeString (fd, kind);
  writeString (fd, s->profiling.stack ? "stack\n" : "current\n");
  writeUint32X (fd, s->magic);
  writeNewline (fd);
  writeUintmaxU (fd, p->total);
  writeString (fd, " ");
  writeUintmaxU (fd, p->totalGC);
  writeNewline (fd);
  writeUint32U (fd, s->profiling.sourcesLength);
  writeNewline (fd);
  for (i = 0; i < s->profiling.sourcesLength; i++)
    profileWriteCount (s, p, fd, i);
  writeUint32U (fd, s->profiling.sourceNamesLength);
  writeNewline (fd);
  for (i = 0; i < s->profiling.sourceNamesLength; i++)
    profileWriteCount (s, p, fd, i + s->profiling.sourcesLength);
}

#if not HAS_TIME_PROFILING

/* No time profiling on this platform.  There is a check in
 * mlton/main/main.fun to make sure that time profiling is never
 * turned on.
 */
static void profileTimeInit (GC_state s) __attribute__ ((noreturn));
static void profileTimeInit (GC_state s) {
  die ("no time profiling");
}

#else

static GC_state catcherState;

void GC_handleSigProf (pointer pc) {
  GC_frameIndex frameIndex;
  GC_state s;
  uint32_t sourceSeqIndex;

  s = catcherState;
  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_handleSigProf ("FMTPTR")\n", (uintptr_t)pc);
  if (s->amInGC)
    sourceSeqIndex = SOURCE_SEQ_GC;
  else {
    frameIndex = topFrameIndex (s, currentThreadStack (s));
    if (C_FRAME == s->frameLayouts[frameIndex].kind)
      sourceSeqIndex = s->profiling.frameSources[frameIndex];
    else {
      if (s->profiling.textStart <= pc and pc < s->profiling.textEnd)
        sourceSeqIndex = s->profiling.textSources [pc - s->profiling.textStart];
      else {
        if (DEBUG_PROFILE)
          fprintf (stderr, "pc out of bounds\n");
        sourceSeqIndex = SOURCE_SEQ_UNKNOWN;
      }
    }
  }
  profileInc (s, 1, sourceSeqIndex);
}

static int compareSourceLabels (const void *v1, const void *v2) {
  uintptr_t ui1;
  uintptr_t ui2;

  ui1 = (uintptr_t)v1;
  ui2 = (uintptr_t)v2;

  if (ui1 < ui2)
    return -1;
  else if (ui1 == ui2)
    return 0;
  else /* if (ui1 > ui2) */
    return 1;
}

static void profileTimeInit (GC_state s) {
  uint32_t i;
  pointer p;
  struct sigaction sa;
  uint32_t sourceSeqsIndex;

  s->profiling.data = GC_profileNew (s);
  /* Sort sourceLabels by address. */
  qsort (s->profiling.sourceLabels, 
         s->profiling.sourceLabelsLength, 
         sizeof (*s->profiling.sourceLabels),
         compareSourceLabels);
  if (0 == s->profiling.sourceLabels[s->profiling.sourceLabelsLength - 1].label)
    die ("Max profile label is 0 -- something is wrong.");
  if (DEBUG_PROFILE)
    for (i = 0; i < s->profiling.sourceLabelsLength; i++)
      fprintf (stderr, FMTPTR"  %"PRIu32"\n",
               (uintptr_t)s->profiling.sourceLabels[i].label,
               s->profiling.sourceLabels[i].sourceSeqsIndex);
  if (ASSERT)
    for (i = 1; i < s->profiling.sourceLabelsLength; i++)
      assert (s->profiling.sourceLabels[i-1].label
              <= s->profiling.sourceLabels[i].label);
  /* Initialize s->textSources. */
  s->profiling.textEnd = (pointer)(getTextEnd());
  s->profiling.textStart = (pointer)(getTextStart());
  if (ASSERT)
    for (i = 0; i < s->profiling.sourceLabelsLength; i++) {
      pointer label;

      label = s->profiling.sourceLabels[i].label;
      assert (0 == label
              or (s->profiling.textStart <= label
                  and label < s->profiling.textEnd));
    }
  s->profiling.textSources =
    (uint32_t*)
    (calloc_safe((size_t)(s->profiling.textEnd - s->profiling.textStart), 
                 sizeof(*(s->profiling.textSources))));
  p = s->profiling.textStart;
  sourceSeqsIndex = SOURCE_SEQ_UNKNOWN;
  for (i = 0; i < s->profiling.sourceLabelsLength; i++) {
    for ( ; p < s->profiling.sourceLabels[i].label; p++)
      s->profiling.textSources[p - s->profiling.textStart] = sourceSeqsIndex;
    sourceSeqsIndex = s->profiling.sourceLabels[i].sourceSeqsIndex;
  }
  for ( ; p < s->profiling.textEnd; p++)
    s->profiling.textSources[p - s->profiling.textStart] = sourceSeqsIndex;
  /*
   * Install catcher, which handles SIGPROF and calls MLton_Profile_inc.
   *
   * One thing I should point out that I discovered the hard way: If
   * the call to sigaction does NOT specify the SA_ONSTACK flag, then
   * even if you have called sigaltstack(), it will NOT switch stacks,
   * so you will probably die.  Worse, if the call to sigaction DOES
   * have SA_ONSTACK and you have NOT called sigaltstack(), it still
   * switches stacks (to location 0) and you die of a SEGV.  Thus the
   * sigaction() call MUST occur after the call to sigaltstack(), and
   * in order to have profiling cover as much as possible, you want it
   * to occur right after the sigaltstack() call.
   */
  catcherState = s;
  sigemptyset (&sa.sa_mask);
  setSigProfHandler (&sa);
  unless (sigaction (SIGPROF, &sa, NULL) == 0)
    diee ("sigaction() failed");
  /* Start the SIGPROF timer. */
  setProfTimer (10000);
}

#endif

/* profileEnd is for writing out an mlmon.out file even if the C code
 * terminates abnormally, e.g. due to running out of memory.  It will
 * only run if the usual SML profile atExit cleanup code did not
 * manage to run.
 */
static GC_state profileEndState;

static void profileEnd (void) {
  int fd;
  GC_state s;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileEnd ()\n");
  s = profileEndState;
  if (s->profiling.isOn) {
    fd = creat ("mlmon.out", 0666);
    if (fd < 0)
      diee ("Cannot create mlmon.out");
    GC_profileWrite (s, s->profiling.data, fd);
  }
}
