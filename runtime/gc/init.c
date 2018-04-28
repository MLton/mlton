/* Copyright (C) 2009,2012,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

static bool stringToBool (char *s) {
  if (0 == strcmp (s, "false"))
    return FALSE;
  if (0 == strcmp (s, "true"))
    return TRUE;
  die ("Invalid @MLton bool: %s.", s);
}

// From gdtoa/gdtoa.h.
// Can't include the whole thing because it brings in too much junk.
float gdtoa__strtof (const char *, char **);

static float stringToFloat (char *s) {
  char *endptr;
  float f;

  f = gdtoa__strtof (s, &endptr);
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

int processAtMLton (GC_state s, int start, int argc, char **argv,
                    char **worldFile) {
  int i;

  i = start;
  while (s->controls.mayProcessAtMLton
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
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton copy-generational-ratio missing argument.");
          s->controls.ratios.copyGenerational = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.copyGenerational)
            die ("@MLton copy-generational-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "copy-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton copy-ratio missing argument.");
          s->controls.ratios.copy = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.copy)
            die ("@MLton copy-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "fixed-heap")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton fixed-heap missing argument.");
          s->controls.fixedHeap = align (stringToBytes (argv[i++]),
                                         2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "gc-messages")) {
          i++;
          s->controls.messages = TRUE;
        } else if (0 == strcmp (arg, "gc-summary")) {
          i++;
          s->controls.summary = TRUE;
        } else if (0 == strcmp (arg, "gc-summary-file")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--")))
            die ("@MLton gc-summary-file missing argument.");
          s->controls.summary = TRUE;
          s->controls.summaryFile = fopen(argv[i++], "w");
          if (s->controls.summaryFile == NULL) {
            die ("Invalid @MLton gc-summary-file %s (%s).", argv[i-1], strerror(errno));
          }
        } else if (0 == strcmp (arg, "grow-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton grow-ratio missing argument.");
          s->controls.ratios.grow = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.grow)
            die ("@MLton grow-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "hash-cons")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton hash-cons missing argument.");
          s->controls.ratios.hashCons = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls.ratios.hashCons
                  and s->controls.ratios.hashCons <= 1.0)
            die ("@MLton hash-cons argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "live-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton live-ratio missing argument.");
          s->controls.ratios.live = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.live)
            die ("@MLton live-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "load-world")) {
          unless (s->controls.mayLoadWorld)
            die ("May not load world.");
          i++;
          s->amOriginal = FALSE;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton load-world missing argument.");
          *worldFile = argv[i++];
        } else if (0 == strcmp (arg, "mark-compact-generational-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton mark-compact-generational-ratio missing argument.");
          s->controls.ratios.markCompactGenerational = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.markCompactGenerational)
            die ("@MLton mark-compact-generational-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "mark-compact-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton mark-compact-ratio missing argument.");
          s->controls.ratios.markCompact = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.markCompact)
            die ("@MLton mark-compact-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "max-heap")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton max-heap missing argument.");
          s->controls.maxHeap = align (stringToBytes (argv[i++]),
                                       2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "may-page-heap")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton may-page-heap missing argument.");
          s->controls.mayPageHeap = stringToBool (argv[i++]);
        } else if (0 == strcmp (arg, "no-load-world")) {
          i++;
          s->controls.mayLoadWorld = FALSE;
        } else if (0 == strcmp (arg, "nursery-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton nursery-ratio missing argument.");
          s->controls.ratios.nursery = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.nursery)
            die ("@MLton nursery-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "ram-slop")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton ram-slop missing argument.");
          s->controls.ratios.ramSlop = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "show-sources")) {
          showSources (s);
          exit (0);
        } else if (0 == strcmp (arg, "stop")) {
          i++;
          s->controls.mayProcessAtMLton = FALSE;
        } else if (0 == strcmp (arg, "stack-current-grow-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-grow-ratio missing argument.");
          s->controls.ratios.stackCurrentGrow = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.stackCurrentGrow)
            die ("@MLton stack-current-grow-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-current-max-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-max-reserved-ratio missing argument.");
          s->controls.ratios.stackCurrentMaxReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.stackCurrentMaxReserved)
            die ("@MLton stack-current-max-reserved-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-current-permit-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-permit-reserved-ratio missing argument.");
          s->controls.ratios.stackCurrentPermitReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.stackCurrentPermitReserved)
            die ("@MLton stack-current-permit-reserved-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-current-shrink-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-shrink-ratio missing argument.");
          s->controls.ratios.stackCurrentShrink = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls.ratios.stackCurrentShrink
                  and s->controls.ratios.stackCurrentShrink <= 1.0)
            die ("@MLton stack-current-shrink-ratio argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "stack-max-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-max-reserved-ratio missing argument.");
          s->controls.ratios.stackMaxReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls.ratios.stackMaxReserved)
            die ("@MLton stack-max-reserved-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-shrink-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-shrink-ratio missing argument.");
          s->controls.ratios.stackShrink = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls.ratios.stackShrink
                  and s->controls.ratios.stackShrink <= 1.0)
            die ("@MLton stack-shrink-ratio argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "use-mmap")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton use-mmap missing argument.");
          GC_setCygwinUseMmap (stringToBool (argv[i++]));
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
  char *worldFile;
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
  s->controls.fixedHeap = 0;
  s->controls.maxHeap = 0;
  s->controls.mayLoadWorld = TRUE;
  s->controls.mayPageHeap = FALSE;
  s->controls.mayProcessAtMLton = TRUE;
  s->controls.messages = FALSE;
  s->controls.oldGenArraySize = 0x100000;
  s->controls.ratios.copy = 4.0f;
  s->controls.ratios.copyGenerational = 4.0f;
  s->controls.ratios.grow = 8.0f;
  s->controls.ratios.hashCons = 0.0f;
  s->controls.ratios.live = 8.0f;
  s->controls.ratios.markCompact = 1.04f;
  s->controls.ratios.markCompactGenerational = 8.0f;
  s->controls.ratios.nursery = 10.0f;
  s->controls.ratios.ramSlop = 0.5f;
  s->controls.ratios.stackCurrentGrow = 2.0f;
  s->controls.ratios.stackCurrentMaxReserved = 32.0f;
  s->controls.ratios.stackCurrentPermitReserved = 4.0f;
  s->controls.ratios.stackCurrentShrink = 0.5f;
  s->controls.ratios.stackMaxReserved = 8.0f;
  s->controls.ratios.stackShrink = 0.5f;
  s->controls.summary = FALSE;
  s->controls.summaryFile = stderr;
  s->cumulativeStatistics.bytesAllocated = 0;
  s->cumulativeStatistics.bytesCopied = 0;
  s->cumulativeStatistics.bytesCopiedMinor = 0;
  s->cumulativeStatistics.bytesHashConsed = 0;
  s->cumulativeStatistics.bytesMarkCompacted = 0;
  s->cumulativeStatistics.bytesScannedMinor = 0;
  s->cumulativeStatistics.maxBytesLive = 0;
  s->cumulativeStatistics.maxHeapSize = 0;
  s->cumulativeStatistics.maxPauseTime = 0;
  s->cumulativeStatistics.maxStackSize = 0;
  s->cumulativeStatistics.numCardsMarked = 0;
  s->cumulativeStatistics.numCopyingGCs = 0;
  s->cumulativeStatistics.numHashConsGCs = 0;
  s->cumulativeStatistics.numMarkCompactGCs = 0;
  s->cumulativeStatistics.numMinorGCs = 0;
  rusageZero (&s->cumulativeStatistics.ru_gc);
  rusageZero (&s->cumulativeStatistics.ru_gcCopying);
  rusageZero (&s->cumulativeStatistics.ru_gcMarkCompact);
  rusageZero (&s->cumulativeStatistics.ru_gcMinor);
  s->currentThread = BOGUS_OBJPTR;
  s->hashConsDuringGC = FALSE;
  initHeap (s, &s->heap);
  s->lastMajorStatistics.bytesHashConsed = 0;
  s->lastMajorStatistics.bytesLive = 0;
  s->lastMajorStatistics.kind = GC_COPYING;
  s->lastMajorStatistics.numMinorGCs = 0;
  s->savedThread = BOGUS_OBJPTR;
  initHeap (s, &s->secondaryHeap);
  s->signalHandlerThread = BOGUS_OBJPTR;
  s->signalsInfo.amInSignalHandler = FALSE;
  s->signalsInfo.gcSignalHandled = FALSE;
  s->signalsInfo.gcSignalPending = FALSE;
  s->signalsInfo.signalIsPending = FALSE;
  sigemptyset (&s->signalsInfo.signalsHandled);
  sigemptyset (&s->signalsInfo.signalsPending);
  s->sysvals.pageSize = GC_pageSize ();
  s->sysvals.physMem = GC_physMem ();
  s->weaks = NULL;
  s->saveWorldStatus = true;

  initIntInf (s);
  initSignalStack (s);
  worldFile = NULL;

  unless (isAligned (s->sysvals.pageSize, CARD_SIZE))
    die ("Page size must be a multiple of card size.");
  processAtMLton (s, 0, s->atMLtonsLength, s->atMLtons, &worldFile);
  res = processAtMLton (s, 1, argc, argv, &worldFile);
  if (s->controls.fixedHeap > 0 and s->controls.maxHeap > 0)
    die ("Cannot use both fixed-heap and max-heap.");
  unless (s->controls.ratios.markCompact <= s->controls.ratios.copy
          and s->controls.ratios.copy <= s->controls.ratios.live)
    die ("Ratios must satisfy mark-compact-ratio <= copy-ratio <= live-ratio.");
  unless (s->controls.ratios.stackCurrentPermitReserved
          <= s->controls.ratios.stackCurrentMaxReserved)
    die ("Ratios must satisfy stack-current-permit-reserved <= stack-current-max-reserved.");
  /* We align s->sysvals.ram by s->sysvals.pageSize so that we can
   * test whether or not we we are using mark-compact by comparing
   * heap size to ram size.  If we didn't round, the size might be
   * slightly off.
   */
  uintmax_t ram;
  ram = alignMax ((uintmax_t)(s->controls.ratios.ramSlop * (double)(s->sysvals.physMem)),
                  (uintmax_t)(s->sysvals.pageSize));
  ram = min (ram, alignMaxDown((uintmax_t)SIZE_MAX, (uintmax_t)(s->sysvals.pageSize)));
  s->sysvals.ram = (size_t)ram;
  if (DEBUG or DEBUG_RESIZING or s->controls.messages)
    fprintf (stderr, "[GC: Found %s bytes of RAM; using %s bytes (%.1f%% of RAM).]\n",
             uintmaxToCommaString(s->sysvals.physMem),
             uintmaxToCommaString(s->sysvals.ram),
             100.0 * ((double)ram / (double)(s->sysvals.physMem)));
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
    assert (invariantForMutator (s, TRUE, FALSE));
  } else {
    loadWorldFromFileName (s, worldFile);
    if (s->profiling.isOn and s->profiling.stack)
      foreachStackFrame (s, enterFrameForProfiling);
    assert (invariantForMutator (s, TRUE, TRUE));
  }
  s->amInGC = FALSE;
  return res;
}
