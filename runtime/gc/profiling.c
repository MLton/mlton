/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

GC_profileMasterIndex sourceIndexToProfileMasterIndex (GC_state s, 
                                                       GC_sourceIndex i)
 {
  return s->sourceMaps.sources[i].sourceNameIndex + s->sourceMaps.sourcesLength;
}

GC_sourceNameIndex profileMasterIndexToSourceNameIndex (GC_state s, 
                                                        GC_profileMasterIndex i) {
  assert (i >= s->sourceMaps.sourcesLength);
  return i - s->sourceMaps.sourcesLength;
}

GC_profileStack getProfileStackInfo (GC_state s, GC_profileMasterIndex i) {
  assert (s->profiling.data != NULL);
  return &(s->profiling.data->stack[i]);
}


static int profileDepth = 0;

static void profileIndent (void) {
  int i;
  
  for (i = 0; i < profileDepth; ++i)
    fprintf (stderr, " ");
}


void addToStackForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;

  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  if (DEBUG_PROFILE)
    fprintf (stderr, "adding %s to stack  lastTotal = %"PRIuMAX"  lastTotalGC = %"PRIuMAX"\n",
             GC_sourceName (s, i), 
             p->total,
             p->totalGC);
  ps->lastTotal = p->total;
  ps->lastTotalGC = p->totalGC;
}

void enterSourceForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;
  
  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  if (0 == ps->numOccurrences) {
    ps->lastTotal = p->total;
    ps->lastTotalGC = p->totalGC;
  }
  ps->numOccurrences++;
}

void enterForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex) {
  uint32_t i;
  GC_profileData p;
  GC_sourceIndex sourceIndex;
  uint32_t *sourceSeq;

  if (DEBUG_PROFILE)
    fprintf (stderr, "enterForProfiling ("FMTSSI")\n", sourceSeqIndex);
  assert (s->profiling.stack);
  assert (sourceSeqIndex < s->sourceMaps.sourceSeqsLength);
  p = s->profiling.data;
  sourceSeq = s->sourceMaps.sourceSeqs[sourceSeqIndex];
  for (i = 1; i <= sourceSeq[0]; i++) {
    sourceIndex = sourceSeq[i];
    if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
      profileIndent ();
      fprintf (stderr, "(entering %s\n",
               GC_sourceName (s, sourceIndex));
      profileDepth++;
    }
    enterSourceForProfiling (s, (GC_profileMasterIndex)sourceIndex);
    enterSourceForProfiling (s, sourceIndexToProfileMasterIndex (s, sourceIndex));
  }
}

void enterFrameForProfiling (GC_state s, GC_frameIndex i) {
  enterForProfiling (s, s->sourceMaps.frameSources[i]);
}

void GC_profileEnter (GC_state s) {
  enterForProfiling (s, getStackTopFrameSourceSeqIndex (s, getStackCurrent (s)));
}

void removeFromStackForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;

  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  if (DEBUG_PROFILE)
    fprintf (stderr, "removing %s from stack  ticksInc = %"PRIuMAX"  ticksGCInc = %"PRIuMAX"\n",
             GC_sourceName (s, i), 
             p->total - ps->lastTotal,
             p->totalGC - ps->lastTotalGC);
  ps->ticks += p->total - ps->lastTotal;
  ps->ticksGC += p->totalGC - ps->lastTotalGC;
}

void leaveSourceForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;

  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  assert (ps->numOccurrences > 0);
  ps->numOccurrences--;
  if (0 == ps->numOccurrences)
    removeFromStackForProfiling (s, i);
}

void leaveForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex) {
  int32_t i;
  GC_profileData p;
  GC_sourceIndex sourceIndex;
  uint32_t *sourceSeq;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileLeave ("FMTSSI")\n", sourceSeqIndex);
  assert (s->profiling.stack);
  assert (sourceSeqIndex < s->sourceMaps.sourceSeqsLength);
  p = s->profiling.data;
  sourceSeq = s->sourceMaps.sourceSeqs[sourceSeqIndex];
  for (i = sourceSeq[0]; i > 0; i--) {
    sourceIndex = sourceSeq[i];
    if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
      profileDepth--;
      profileIndent ();
      fprintf (stderr, "leaving %s)\n",
               GC_sourceName (s, sourceIndex));
    }
    leaveSourceForProfiling (s, (GC_profileMasterIndex)sourceIndex);
    leaveSourceForProfiling (s, sourceIndexToProfileMasterIndex (s, sourceIndex));
  }
}

void leaveFrameForProfiling (GC_state s, GC_frameIndex i) {
  leaveForProfiling (s, s->sourceMaps.frameSources[i]);
}

void GC_profileLeave (GC_state s) {
  leaveForProfiling (s, getStackTopFrameSourceSeqIndex (s, getStackCurrent (s)));
}


void profileInc (GC_state s, size_t amount, GC_sourceSeqIndex sourceSeqIndex) {
  uint32_t *sourceSeq;
  GC_sourceIndex topSourceIndex;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileInc (%zu, "FMTSSI")\n",
             amount, sourceSeqIndex);
  assert (sourceSeqIndex < s->sourceMaps.sourceSeqsLength);
  sourceSeq = s->sourceMaps.sourceSeqs[sourceSeqIndex];
  topSourceIndex = 
    sourceSeq[0] > 0 
    ? sourceSeq[sourceSeq[0]] 
    : SOURCES_INDEX_UNKNOWN;
  if (DEBUG_PROFILE) {
    profileIndent ();
    fprintf (stderr, "bumping %s by %zu\n",
             GC_sourceName (s, topSourceIndex), amount);
  }
  s->profiling.data->countTop[topSourceIndex] += amount;
  s->profiling.data->countTop[sourceIndexToProfileMasterIndex (s, topSourceIndex)] += amount;
  if (s->profiling.stack)
    enterForProfiling (s, sourceSeqIndex);
  if (SOURCES_INDEX_GC == topSourceIndex)
    s->profiling.data->totalGC += amount;
  else
    s->profiling.data->total += amount;
  if (s->profiling.stack)
    leaveForProfiling (s, sourceSeqIndex);
}

void GC_profileInc (GC_state s, size_t amount) {
  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_profileInc (%zu)\n", amount);
  profileInc (s, amount,
              s->amInGC
              ? SOURCE_SEQ_GC
              : getStackTopFrameSourceSeqIndex (s, getStackCurrent (s)));
}

void GC_profileAllocInc (GC_state s, size_t amount) {
  if (s->profiling.isOn and (PROFILE_ALLOC == s->profiling.kind)) {
    if (DEBUG_PROFILE)
      fprintf (stderr, "GC_profileAllocInc (%zu)\n", amount);
    GC_profileInc (s, amount);
  }
}


GC_profileData GC_profileMalloc (GC_state s) {
  GC_profileData p;
  uint32_t profileMasterLength;

  p = (GC_profileData)(malloc_safe (sizeof(*p)));
  p->total = 0;
  p->totalGC = 0;
  profileMasterLength = s->sourceMaps.sourcesLength + s->sourceMaps.sourceNamesLength;
  p->countTop = (uintmax_t*)(calloc_safe(profileMasterLength, sizeof(*(p->countTop))));
  if (s->profiling.stack)
    p->stack =
      (struct GC_profileStack *)
      (calloc_safe(profileMasterLength, sizeof(*(p->stack))));
  if (DEBUG_PROFILE)
    fprintf (stderr, FMTPTR" = GC_profileMalloc ()\n", (uintptr_t)p);
  return p;
}

void GC_profileFree (GC_state s, GC_profileData p) {
  free (p->countTop);
  if (s->profiling.stack)
    free (p->stack);
  free (p);
}

static void writeProfileCount (GC_state s, int fd,
                               GC_profileData p, GC_profileMasterIndex i) {
  writeUintmaxU (fd, p->countTop[i]);
  if (s->profiling.stack) {
    GC_profileStack ps;
    
    ps = &(p->stack[i]);
    writeString (fd, " ");
    writeUintmaxU (fd, ps->ticks);
    writeString (fd, " ");
    writeUintmaxU (fd, ps->ticksGC);
  }
  writeNewline (fd);
}

void GC_profileWrite (GC_state s, GC_profileData p, int fd) {
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
  writeUint32U (fd, s->sourceMaps.sourcesLength);
  writeNewline (fd);
  for (GC_sourceIndex i = 0; i < s->sourceMaps.sourcesLength; i++)
    writeProfileCount (s, fd, p, 
                       (GC_profileMasterIndex)i);
  writeUint32U (fd, s->sourceMaps.sourceNamesLength);
  writeNewline (fd);
  for (GC_sourceNameIndex i = 0; i < s->sourceMaps.sourceNamesLength; i++)
    writeProfileCount (s, fd, p,  
                       (GC_profileMasterIndex)(i + s->sourceMaps.sourcesLength));
}


void setProfTimer (long usec) {
  struct itimerval iv;
  
  iv.it_interval.tv_sec = 0;
  iv.it_interval.tv_usec = usec;
  iv.it_value.tv_sec = 0;
  iv.it_value.tv_usec = usec;
  unless (0 == setitimer (ITIMER_PROF, &iv, NULL))
    die ("setProfTimer: setitimer failed");
}

#if not HAS_TIME_PROFILING

/* No time profiling on this platform.  There is a check in
 * mlton/main/main.fun to make sure that time profiling is never
 * turned on.
 */
void initProfilingTime (GC_state s) __attribute__ ((noreturn));
void initProfilingTime (__attribute__ ((unused)) GC_state s) {
  die ("no time profiling");
}

#else

static GC_state handleSigProfState;

void GC_handleSigProf (pointer pc) {
  GC_frameIndex frameIndex;
  GC_state s;
  GC_sourceSeqIndex sourceSeqIndex;

  s = handleSigProfState;
  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_handleSigProf ("FMTPTR")\n", (uintptr_t)pc);
  if (s->amInGC)
    sourceSeqIndex = SOURCE_SEQ_GC;
  else {
    frameIndex = getStackTopFrameIndex (s, getStackCurrent (s));
    if (C_FRAME == s->frameLayouts[frameIndex].kind)
      sourceSeqIndex = s->sourceMaps.frameSources[frameIndex];
    else {
      if (s->sourceMaps.textStart <= pc and pc < s->sourceMaps.textEnd)
        sourceSeqIndex = s->sourceMaps.textSources [pc - s->sourceMaps.textStart];
      else {
        if (DEBUG_PROFILE)
          fprintf (stderr, "pc out of bounds\n");
        sourceSeqIndex = SOURCE_SEQ_UNKNOWN;
      }
    }
  }
  profileInc (s, 1, sourceSeqIndex);
}

static void initProfilingTime (GC_state s) {
  struct sigaction sa;

  s->profiling.data = GC_profileMalloc (s);
  initTextSources (s);
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
  handleSigProfState = s;
  sigemptyset (&sa.sa_mask);
  GC_setSigProfHandler (&sa);
  unless (sigaction (SIGPROF, &sa, NULL) == 0)
    diee ("initProfilingTime: sigaction failed");
  /* Start the SIGPROF timer. */
  setProfTimer (10000);
}

#endif

/* atexitForProfiling is for writing out an mlmon.out file even if the C code
 * terminates abnormally, e.g. due to running out of memory.  It will
 * only run if the usual SML profile atExit cleanup code did not
 * manage to run.
 */
static GC_state atexitForProfilingState;

static void atexitForProfiling (void) {
  int fd;
  GC_state s;

  if (DEBUG_PROFILE)
    fprintf (stderr, "atexitForProfiling ()\n");
  s = atexitForProfilingState;
  if (s->profiling.isOn) {
    fd = creat ("mlmon.out", 0666);
    if (fd < 0)
      diee ("Cannot create mlmon.out");
    GC_profileWrite (s, s->profiling.data, fd);
  }
}

void initProfiling (GC_state s) {
  if (PROFILE_NONE == s->profiling.kind)
    s->profiling.isOn = FALSE;
  else {
    s->profiling.isOn = TRUE;
    assert (s->sourceMaps.frameSourcesLength == s->frameLayoutsLength);
    switch (s->profiling.kind) {
    case PROFILE_ALLOC:
    case PROFILE_COUNT:
      s->profiling.data = GC_profileMalloc (s);
      break;
    case PROFILE_NONE:
      die ("impossible PROFILE_NONE");
    case PROFILE_TIME:
      initProfilingTime (s);
      break;
    }
    atexitForProfilingState = s;
    atexit (atexitForProfiling);
  }
}

void GC_profileDone (GC_state s) {
  GC_profileData p;
  GC_profileMasterIndex profileMasterIndex;

  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_profileDone ()\n");
  assert (s->profiling.isOn);
  if (PROFILE_TIME == s->profiling.kind)
    setProfTimer (0);
  s->profiling.isOn = FALSE;
  p = s->profiling.data;
  if (s->profiling.stack) {
    uint32_t profileMasterLength = 
      s->sourceMaps.sourcesLength + s->sourceMaps.sourceNamesLength;
    for (profileMasterIndex = 0;
         profileMasterIndex < profileMasterLength;
         profileMasterIndex++) {
      if (p->stack[profileMasterIndex].numOccurrences > 0) {
        if (DEBUG_PROFILE)
          fprintf (stderr, "done leaving %s\n",
                   (profileMasterIndex < s->sourceMaps.sourcesLength)
                   ? GC_sourceName (s, (GC_sourceIndex)profileMasterIndex)
                   : s->sourceMaps.sourceNames[
                     profileMasterIndexToSourceNameIndex (s, profileMasterIndex)]);
        removeFromStackForProfiling (s, profileMasterIndex);
      }
    }
  }
}


GC_profileData GC_getProfileCurrent (GC_state s) {
  return s->profiling.data;
}
void GC_setProfileCurrent (GC_state s, GC_profileData p) {
  s->profiling.data = p;
}

