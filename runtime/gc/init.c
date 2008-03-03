/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

#if FALSE
static bool stringToBool (char *s) {
  if (0 == strcmp (s, "false"))
    return FALSE;
  if (0 == strcmp (s, "true"))
    return TRUE;
  die ("Invalid @MLton bool: %s.", s);
}
#endif

// From gdtoa/gdtoa.h.
// Can't include the whole thing because it brings in too much junk.
float gdtoa_strtof (const char *, char **);

static float stringToFloat (char *s) {
  char *endptr;
  float f;

  f = gdtoa_strtof (s, &endptr);
  if (s == endptr)
    die ("Invalid @MLton float: %s.", s);
  return f;
}

static size_t stringToBytes (char *s) {
  double d;
  char *endptr;
  size_t factor;

  d = strtod (s, &endptr);
  if (s == endptr)
    goto bad;
  switch (*endptr++) {
  case 'g':
  case 'G':
    factor = 1024 * 1024 * 1024;
    break;
  case 'k':
  case 'K':
    factor = 1024;
    break;
  case 'm':
  case 'M':
    factor = 1024 * 1024;
    break;
  default:
    goto bad;
  }
  d *= factor;
  unless (*endptr == '\0'
          and 0.0 <= d
          and d <= (double)SIZE_MAX)
    goto bad;
  return (size_t)d;
bad:
  die ("Invalid @MLton memory amount: %s.", s);
}

/* ---------------------------------------------------------------- */
/*                             GC_init                              */
/* ---------------------------------------------------------------- */

int processAtMLton (GC_state s, int argc, char **argv,
                    char **worldFile) {
  int i;

  i = 1;
  while (s->controls->mayProcessAtMLton
         and i < argc
         and (0 == strcmp (argv [i], "@MLton"))) {
    bool done;

    i++;
    done = FALSE;
    while (!done) {
      if (i == argc)
        die ("Missing -- at end of @MLton args.");
      else {
        char *arg;

        arg = argv[i];
        if (0 == strcmp (arg, "copy-generational-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton copy-generational-ratio missing argument.");
          s->controls->ratios.copyGenerational = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.copyGenerational)
            die ("@MLton copy-generational-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "copy-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton copy-ratio missing argument.");
          s->controls->ratios.copy = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.copy)
            die ("@MLton copy-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "fixed-heap")) {
          i++;
          if (i == argc)
            die ("@MLton fixed-heap missing argument.");
          s->controls->fixedHeap = align (stringToBytes (argv[i++]),
                                         2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "gc-messages")) {
          i++;
          s->controls->messages = TRUE;
        } else if (0 == strcmp (arg, "gc-summary")) {
          i++;
#if (defined (__MINGW32__))
          fprintf (stderr, "Warning: MinGW doesn't support gc-summary.\n");
#else
          s->controls.summary = TRUE;
#endif
        } else if (0 == strcmp (arg, "grow-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton grow-ratio missing argument.");
          s->controls->ratios.grow = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.grow)
            die ("@MLton grow-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "hash-cons")) {
          i++;
          if (i == argc)
            die ("@MLton hash-cons missing argument.");
          s->controls->ratios.hashCons = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls->ratios.hashCons
                  and s->controls->ratios.hashCons <= 1.0)
            die ("@MLton hash-cons argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "live-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton live-ratio missing argument.");
          s->controls->ratios.live = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.live)
            die ("@MLton live-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "load-world")) {
          unless (s->controls->mayLoadWorld)
            die ("May not load world.");
          i++;
          s->amOriginal = FALSE;
          if (i == argc)
            die ("@MLton load-world missing argument.");
          *worldFile = argv[i++];
        } else if (0 == strcmp (arg, "mark-compact-generational-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton mark-compact-generational-ratio missing argument.");
          s->controls->ratios.markCompactGenerational = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.markCompactGenerational)
            die ("@MLton mark-compact-generational-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "mark-compact-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton mark-compact-ratio missing argument.");
          s->controls->ratios.markCompact = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.markCompact)
            die ("@MLton mark-compact-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "max-heap")) {
          i++;
          if (i == argc)
            die ("@MLton max-heap missing argument.");
          s->controls->maxHeap = align (stringToBytes (argv[i++]),
                                       2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "no-load-world")) {
          i++;
          s->controls->mayLoadWorld = FALSE;
        } else if (0 == strcmp (arg, "nursery-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton nursery-ratio missing argument.");
          s->controls->ratios.nursery = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.nursery)
            die ("@MLton nursery-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "ram-slop")) {
          i++;
          if (i == argc)
            die ("@MLton ram-slop missing argument.");
          s->controls->ratios.ramSlop = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "show-sources")) {
          showSources (s);
          exit (0);
        } else if (0 == strcmp (arg, "stop")) {
          i++;
          s->controls->mayProcessAtMLton = FALSE;
        } else if (0 == strcmp (arg, "thread-shrink-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton thread-shrink-ratio missing argument.");
          s->controls->ratios.threadShrink = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls->ratios.threadShrink
                  and s->controls->ratios.threadShrink <= 1.0)
            die ("@MLton thread-shrink-ratio argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "use-mmap")) {
          i++;
          GC_setCygwinUseMmap (TRUE);
        } else if (0 == strcmp (arg, "--")) {
          i++;
          done = TRUE;
        } else if (i > 1)
          die ("Strange @MLton arg: %s", argv[i]);
        else done = TRUE;
      }
    }
  }
  return i;
}

int GC_init (GC_state s, int argc, char **argv) {
  int res;

  assert (s->alignment >= GC_MODEL_MINALIGN);
  assert (isAligned (sizeof (struct GC_stack), s->alignment));
  // While the following asserts are manifestly true,
  // they check the asserts in sizeofThread and sizeofWeak.
  assert (sizeofThread (s) == sizeofThread (s));
  assert (sizeofWeak (s) == sizeofWeak (s));

  s->amInGC = TRUE;
  s->amOriginal = TRUE;
  s->atomicState = 0;
  s->callFromCHandlerThread = BOGUS_OBJPTR;
  s->controls = (struct GC_controls *) malloc (sizeof (struct GC_controls));
  s->controls->fixedHeap = 0;
  s->controls->maxHeap = 0;
  s->controls->mayLoadWorld = TRUE;
  s->controls->mayProcessAtMLton = TRUE;
  s->controls->messages = FALSE;
  s->controls->oldGenArraySize = 0x100000;
  s->controls->allocChunkSize = 4096;
  s->controls->affinityBase = 0;
  s->controls->affinityStride = 1;
  s->controls->restrictAvailableSize = FALSE;
  s->controls->ratios.copy = 4.0;
  s->controls->ratios.copyGenerational = 4.0;
  s->controls->ratios.grow = 8.0;
  s->controls->ratios.hashCons = 0.0;
  s->controls->ratios.live = 8.0;
  s->controls->ratios.markCompact = 1.04;
  s->controls->ratios.markCompactGenerational = 8.0;
  s->controls->ratios.nursery = 10.0;
  s->controls->ratios.ramSlop = 0.5;
  s->controls->ratios.threadShrink = 0.5;
  s->controls->ratios.available = 1.1;
  s->controls->rusageMeasureGC = FALSE;
  s->controls->summary = FALSE;
  s->cumulativeStatistics = (struct GC_cumulativeStatistics *) 
    malloc (sizeof (struct GC_cumulativeStatistics));
  s->cumulativeStatistics->bytesAllocated = 0;
  s->cumulativeStatistics->bytesFilled = 0;
  s->cumulativeStatistics->bytesCopied = 0;
  s->cumulativeStatistics->bytesCopiedMinor = 0;
  s->cumulativeStatistics->bytesMarkCompacted = 0;
  s->cumulativeStatistics->markedCards = 0;
  s->cumulativeStatistics->maxBytesLive = 0;
  s->cumulativeStatistics->maxBytesLiveSinceReset = 0;
  s->cumulativeStatistics->maxHeapSizeSeen = 0;
  s->cumulativeStatistics->maxStackSizeSeen = 0;
  s->cumulativeStatistics->minorBytesScanned = 0;
  s->cumulativeStatistics->numLimitChecks = 0;
  s->cumulativeStatistics->syncForOldGenArray = 0;
  s->cumulativeStatistics->syncForNewGenArray = 0;
  s->cumulativeStatistics->syncForStack = 0;
  s->cumulativeStatistics->syncForHeap = 0;
  s->cumulativeStatistics->syncMisc = 0;
  s->cumulativeStatistics->numCopyingGCs = 0;
  s->cumulativeStatistics->numHashConsGCs = 0;
  s->cumulativeStatistics->numMarkCompactGCs = 0;
  s->cumulativeStatistics->numMinorGCs = 0;
  s->cumulativeStatistics->maxPause = 0;
  timevalZero (&s->cumulativeStatistics->tv_gc);
  rusageZero (&s->cumulativeStatistics->ru_gcCopy);
  rusageZero (&s->cumulativeStatistics->ru_gcMarkCompact);
  rusageZero (&s->cumulativeStatistics->ru_gcMinor);
  timevalZero (&s->cumulativeStatistics->tv_sync);
  rusageZero (&s->cumulativeStatistics->ru_thread);
  timevalZero (&s->cumulativeStatistics->tv_rt);
  s->currentThread = BOGUS_OBJPTR;
  s->hashConsDuringGC = FALSE;
  s->heap = (GC_heap) malloc (sizeof (struct GC_heap));
  initHeap (s, s->heap);
  s->lastMajorStatistics = (struct GC_lastMajorStatistics *) 
    malloc (sizeof (struct GC_lastMajorStatistics));
  s->lastMajorStatistics->bytesHashConsed = 0;
  s->lastMajorStatistics->bytesLive = 0;
  s->lastMajorStatistics->kind = GC_COPYING;
  s->lastMajorStatistics->numMinorGCs = 0;
  s->numberOfProcs = 1;
  s->procStates = NULL;
  s->roots = NULL;
  s->rootsLength = 0;
  s->savedThread = BOGUS_OBJPTR;
  s->secondaryHeap = (GC_heap) malloc (sizeof (struct GC_heap));
  initHeap (s, s->secondaryHeap);
  s->signalHandlerThread = BOGUS_OBJPTR;
  s->signalsInfo.amInSignalHandler = FALSE;
  s->signalsInfo.gcSignalHandled = FALSE;
  s->signalsInfo.gcSignalPending = FALSE;
  s->signalsInfo.signalIsPending = FALSE;
  sigemptyset (&s->signalsInfo.signalsHandled);
  sigemptyset (&s->signalsInfo.signalsPending);
  s->startTime = getCurrentTime ();
  s->sysvals.totalRam = GC_totalRam ();
  s->sysvals.pageSize = GC_pageSize ();
  s->weaks = NULL;
  s->saveWorldStatus = true;

  initSignalStack (s);
  s->worldFile = NULL;

  unless (isAligned (s->sysvals.pageSize, CARD_SIZE))
    die ("Page size must be a multiple of card size.");
  processAtMLton (s, s->atMLtonsLength, s->atMLtons, &s->worldFile);
  res = processAtMLton (s, argc, argv, &s->worldFile);
  if (s->controls->fixedHeap > 0 and s->controls->maxHeap > 0)
    die ("Cannot use both fixed-heap and max-heap.");
  unless (s->controls->ratios.markCompact <= s->controls->ratios.copy
          and s->controls->ratios.copy <= s->controls->ratios.live)
    die ("Ratios must satisfy mark-compact-ratio <= copy-ratio <= live-ratio.");
  /* We align s->ram by pageSize so that we can test whether or not we
   * we are using mark-compact by comparing heap size to ram size.  If
   * we didn't round, the size might be slightly off.
   */
  s->sysvals.ram = align ((size_t)(s->controls->ratios.ramSlop * s->sysvals.totalRam), 
                          s->sysvals.pageSize);
  if (DEBUG or DEBUG_RESIZING or s->controls->messages)
    fprintf (stderr, "[GC: total RAM = %s, using RAM = %s.]\n",
             uintmaxToCommaString(s->sysvals.totalRam),
             uintmaxToCommaString(s->sysvals.ram));
  if (DEBUG_SOURCES or DEBUG_PROFILE) {
    uint32_t i;
    for (i = 0; i < s->sourceMaps.frameSourcesLength; i++) {
      uint32_t j;
      uint32_t *sourceSeq;
      fprintf (stderr, "%"PRIu32"\n", i);
      sourceSeq = s->sourceMaps.sourceSeqs[s->sourceMaps.frameSources[i]];
      for (j = 1; j <= sourceSeq[0]; j++)
        fprintf (stderr, "\t%s\n",
                 s->sourceMaps.sourceNames[
                 s->sourceMaps.sources[sourceSeq[j]].sourceNameIndex
                 ]);
    }
  }
  return res;
}

void GC_lateInit (GC_state s) {
  /* Initialize profiling.  This must occur after processing
   * command-line arguments, because those may just be doing a
   * show-sources, in which case we don't want to initialize the
   * atExit.
   */
  initProfiling (s);
  if (s->amOriginal) {
    initWorld (s);
    /* The mutator stack invariant doesn't hold,
     * because the mutator has yet to run.
     */
    // spoons: can't assert because other threads are init'd
    //assert (invariantForMutator (s, TRUE, FALSE));
  } else {
    loadWorldFromFileName (s, s->worldFile);
    if (s->profiling.isOn and s->profiling.stack)
      foreachStackFrame (s, enterFrameForProfiling);
    assert (invariantForMutator (s, TRUE, TRUE));
  }
  s->amInGC = FALSE;
}

void GC_duplicate (GC_state d, GC_state s) {
  // GC_init
  d->amInGC = s->amInGC;
  d->amOriginal = s->amOriginal;
  d->atomicState = 0;
  d->callFromCHandlerThread = BOGUS_OBJPTR;
  d->controls = s->controls;
  d->cumulativeStatistics = s->cumulativeStatistics;
  d->currentThread = BOGUS_OBJPTR;
  d->hashConsDuringGC = s->hashConsDuringGC;
  d->lastMajorStatistics = s->lastMajorStatistics;
  d->numberOfProcs = s->numberOfProcs;
  d->roots = NULL;
  d->rootsLength = 0;
  d->savedThread = BOGUS_OBJPTR;
  d->signalHandlerThread = BOGUS_OBJPTR;
  d->signalsInfo.amInSignalHandler = FALSE;
  d->signalsInfo.gcSignalHandled = FALSE;
  d->signalsInfo.gcSignalPending = FALSE;
  d->signalsInfo.signalIsPending = FALSE;
  sigemptyset (&d->signalsInfo.signalsHandled);
  sigemptyset (&d->signalsInfo.signalsPending);
  d->startTime = s->startTime;
  d->syncReason = SYNC_NONE;
  d->sysvals.totalRam = s->sysvals.totalRam;
  d->sysvals.pageSize = s->sysvals.pageSize;
  d->weaks = s->weaks;
  d->saveWorldStatus = s->saveWorldStatus;

  // XXX spoons better duplicate?
  //initSignalStack (d);

  d->sysvals.ram = s->sysvals.ram;

  // XXX spoons better duplicate
  //initProfiling (d);

  // Multi-processor support is incompatible with saved-worlds
  assert (d->amOriginal);
  duplicateWorld (d, s);
  s->amInGC = FALSE;
}
