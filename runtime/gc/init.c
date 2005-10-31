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

static void initSignalStack (GC_state s) {
#if HAS_SIGALTSTACK
  static stack_t altstack;
  size_t ss_size = align (SIGSTKSZ, s->sysvals.pageSize);
  size_t psize = s->sysvals.pageSize;
  void *ss_sp = GC_mmap_safe_protect (NULL, 2 * ss_size, psize, psize);
  altstack.ss_sp = (unsigned char*)ss_sp + ss_size;
  altstack.ss_size = ss_size;
  altstack.ss_flags = 0;
  sigaltstack (&altstack, NULL);
#endif
}

#if FALSE
static bool stringToBool (char *s) {
  if (0 == strcmp (s, "false"))
    return FALSE;
  if (0 == strcmp (s, "true"))
    return TRUE;
  die ("Invalid @MLton bool: %s.", s);
}
#endif

static float stringToFloat (char *s) {
  char *endptr;
  float f;

  f = strtof (s, &endptr);
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

static void setInitialBytesLive (GC_state s) {
  uint32_t i;
  size_t numBytes;
  
  s->lastMajorStatistics.bytesLive = 0;
  for (i = 0; i < s->intInfInitsLength; ++i) {
    numBytes = 
      sizeof(uint32_t) // for the sign
      + strlen (s->intInfInits[i].mlstr);
    s->lastMajorStatistics.bytesLive +=
      align (GC_ARRAY_HEADER_SIZE + numBytes,
             s->alignment);
  }
  for (i = 0; i < s->vectorInitsLength; ++i) {
    numBytes = 
      s->vectorInits[i].bytesPerElement
      * s->vectorInits[i].numElements;
    s->lastMajorStatistics.bytesLive +=
      align (GC_ARRAY_HEADER_SIZE
             + ((0 == numBytes)
                ? OBJPTR_SIZE
                : numBytes),
             s->alignment);
  }
}

static void initIntInfs (GC_state s) {
  struct GC_intInfInit *inits;
  pointer frontier;
  char *str;
  size_t slen, llen;
  mp_size_t alen;
  uint32_t i, j;
  bool neg, hex;
  GC_intInf bp;
  unsigned char *cp;

  assert (isAlignedFrontier (s, s->frontier));
  frontier = s->frontier;
  for (i= 0; i < s->intInfInitsLength; i++) {
    inits = &s->intInfInits[i];
    str = inits->mlstr;
    assert (inits->globalIndex < s->globalsLength);
    neg = *str == '~';
    if (neg)
      str++;
    slen = strlen (str);
    hex = str[0] == '0' && str[1] == 'x';
    if (hex) {
      str += 2;
      slen -= 2;
      llen = (slen + 7) / 8;
    } else
      llen = (slen + 8) / 9;
    assert (slen > 0);
    bp = (GC_intInf)frontier;
    cp = (unsigned char *)&bp->limbs[llen];

    for (j = 0; j != slen; j++)
      if ('0' <= str[j] && str[j] <= '9')
        cp[j] = str[j] - '0' + 0;
      else if ('a' <= str[j] && str[j] <= 'f')
        cp[j] = str[j] - 'a' + 0xa;
      else {
        assert('A' <= str[j] && str[j] <= 'F');
        cp[j] = str[j] - 'A' + 0xA;
      }
    alen = mpn_set_str ((mp_limb_t*)(bp->limbs), cp, slen, hex ? 0x10 : 10);
    assert ((size_t)alen <= llen);
    if (alen <= 1) {
      uint32_t val, ans;
      
      if (alen == 0)
        val = 0;
      else
        val = bp->limbs[0];
      if (neg) {
        /*
         * We only fit if val in [1, 2^30].
         */
        ans = - val;
        val = val - 1;
      } else
        /* 
         * We only fit if val in [0, 2^30 - 1].
         */
        ans = val;
      if (val < (uint32_t)1<<30) {
        s->globals[inits->globalIndex] = (objptr)(ans<<1 | 1);
        continue;
      }
    }
    s->globals[inits->globalIndex] = pointerToObjptr((pointer)(&bp->isneg), s->heap.start);
    bp->counter = 0;
    bp->length = alen + 1;
    bp->header = buildHeaderFromTypeIndex (WORD32_VECTOR_TYPE_INDEX);
    bp->isneg = neg;
    frontier = alignFrontier (s, (pointer)&bp->limbs[alen]);
  }
  assert (isAlignedFrontier (s, frontier));
  GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->frontier = frontier;
  s->cumulativeStatistics.bytesAllocated += frontier - s->frontier;
}

static void initVectors (GC_state s) {
  struct GC_vectorInit *inits;
  pointer frontier;
  uint32_t i;

  assert (isAlignedFrontier (s, s->frontier));
  inits = s->vectorInits;
  frontier = s->frontier;
  for (i = 0; i < s->vectorInitsLength; i++) {
    size_t bytesPerElement;
    size_t dataBytes;
    size_t objectSize;
    uint32_t typeIndex;

    bytesPerElement = inits[i].bytesPerElement;
    dataBytes = bytesPerElement * inits[i].numElements;
    objectSize = align (GC_ARRAY_HEADER_SIZE
                        + ((0 == dataBytes)
                           ? POINTER_SIZE
                           : dataBytes),
                        s->alignment);
    assert (objectSize <= (size_t)(s->heap.start + s->heap.size - frontier));
    *((GC_arrayCounter*)(frontier)) = 0;
    frontier = frontier + GC_ARRAY_COUNTER_SIZE;
    *((GC_arrayLength*)(frontier)) = inits[i].numElements;
    frontier = frontier + GC_ARRAY_LENGTH_SIZE;
    switch (bytesPerElement) {
    case 1:
      typeIndex = WORD8_VECTOR_TYPE_INDEX;
      break;
    case 2:
      typeIndex = WORD16_VECTOR_TYPE_INDEX;
      break;
    case 4:
      typeIndex = WORD32_VECTOR_TYPE_INDEX;
      break;
    default:
      die ("unknown bytes per element in vectorInit: %zu",
           bytesPerElement);
    }
    *((GC_header*)(frontier)) = buildHeaderFromTypeIndex (typeIndex);
    frontier = frontier + GC_HEADER_SIZE;
    s->globals[inits[i].globalIndex] = pointerToObjptr(frontier, s->heap.start);
    if (DEBUG_DETAILED)
      fprintf (stderr, "allocated vector at "FMTPTR"\n",
               (uintptr_t)(s->globals[inits[i].globalIndex]));
    GC_memcpy (inits[i].bytes, frontier, dataBytes);
    frontier += objectSize - GC_ARRAY_HEADER_SIZE;
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "frontier after string allocation is "FMTPTR"\n",
             (uintptr_t)frontier);
  GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->cumulativeStatistics.bytesAllocated += (size_t)(frontier - s->frontier);
  assert (isAlignedFrontier (s, frontier));
  s->frontier = frontier;
}

static void newWorld (GC_state s) {
  uint32_t i;
  pointer start;
  GC_thread thread;
  
  for (i = 0; i < s->globalsLength; ++i)
    s->globals[i] = BOGUS_OBJPTR;
  setInitialBytesLive (s);
  createHeap (s, &s->heap, 
              sizeofHeapDesired (s, s->lastMajorStatistics.bytesLive, 0),
              s->lastMajorStatistics.bytesLive);
  createCardMapAndCrossMap (s);
  start = alignFrontier (s, s->heap.start);
  s->frontier = start;
  initIntInfs (s);
  initVectors (s);
  assert ((size_t)(s->frontier - start) <= s->lastMajorStatistics.bytesLive);
  s->heap.oldGenSize = s->frontier - s->heap.start;
  setHeapNursery (s, 0, 0);
  thread = newThread (s, sizeofStackInitial (s));
  switchToThread (s, pointerToObjptr((pointer)thread, s->heap.start));
}

/* ---------------------------------------------------------------- */
/*                             GC_init                              */
/* ---------------------------------------------------------------- */

bool MLton_Platform_CygwinUseMmap;

static int processAtMLton (GC_state s, int argc, char **argv,
                           char **worldFile) {
  int i;

  i = 1;
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
        if (0 == strcmp (arg, "copy-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton copy-ratio missing argument.");
          s->ratios.copy = stringToFloat (argv[i++]);
        } else if (0 == strcmp(arg, "fixed-heap")) {
          i++;
          if (i == argc)
            die ("@MLton fixed-heap missing argument.");
          s->controls.fixedHeap = align (stringToBytes (argv[i++]),
                                         2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "gc-messages")) {
          i++;
          s->controls.messages = TRUE;
        } else if (0 == strcmp (arg, "gc-summary")) {
          i++;
#if (defined (__MINGW32__))
          fprintf (stderr, "Warning: MinGW doesn't yet support gc-summary\n");
#else
          s->controls.summary = TRUE;
#endif
        } else if (0 == strcmp (arg, "copy-generational-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton copy-generational-ratio missing argument.");
          s->ratios.copyGenerational = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "grow-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton grow-ratio missing argument.");
          s->ratios.grow = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "hash-cons")) {
          i++;
          if (i == argc)
            die ("@MLton hash-cons missing argument.");
          s->ratios.hashCons = stringToFloat (argv[i++]);
          unless (0.0 <= s->ratios.hashCons
                  and s->ratios.hashCons <= 1.0)
            die ("@MLton hash-cons argument must be between 0.0 and 1.0");
        } else if (0 == strcmp (arg, "live-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton live-ratio missing argument.");
          s->ratios.live = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "load-world")) {
          unless (s->controls.mayLoadWorld)
            die ("May not load world.");
          i++;
          s->amOriginal = FALSE;
          if (i == argc)
            die ("@MLton load-world missing argument.");
          *worldFile = argv[i++];
        } else if (0 == strcmp (arg, "max-heap")) {
          i++;
          if (i == argc)
            die ("@MLton max-heap missing argument.");
          s->controls.maxHeap = align (stringToBytes (argv[i++]),
                                       2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "mark-compact-generational-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton mark-compact-generational-ratio missing argument.");
          s->ratios.markCompactGenerational = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "mark-compact-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton mark-compact-ratio missing argument.");
          s->ratios.markCompact = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "no-load-world")) {
          i++;
          s->controls.mayLoadWorld = FALSE;
        } else if (0 == strcmp (arg, "nursery-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton nursery-ratio missing argument.");
          s->ratios.nursery = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "ram-slop")) {
          i++;
          if (i == argc)
            die ("@MLton ram-slop missing argument.");
          s->ratios.ramSlop = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "show-prof")) {
          showProf (s);
          exit (0);
        } else if (0 == strcmp (arg, "stop")) {
          i++;
          s->controls.mayProcessAtMLton = FALSE;
        } else if (0 == strcmp (arg, "thread-shrink-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton thread-shrink-ratio missing argument.");
          s->ratios.threadShrink = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "use-mmap")) {
          i++;
          MLton_Platform_CygwinUseMmap = TRUE;
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

  assert (isAligned (sizeof (struct GC_stack), s->alignment));
  assert (isAligned (GC_NORMAL_HEADER_SIZE + sizeof (struct GC_thread),
                     s->alignment));
  assert (isAligned (GC_NORMAL_HEADER_SIZE + sizeof (struct GC_weak),
                     s->alignment));

  s->amInGC = TRUE;
  s->amOriginal = TRUE;
  s->atomicState = 0;
  s->callFromCHandlerThread = BOGUS_OBJPTR;
  s->controls.fixedHeap = 0;
  s->controls.maxHeap = 0;
  s->controls.mayLoadWorld = TRUE;
  s->controls.mayProcessAtMLton = TRUE;
  s->controls.messages = FALSE;
  s->controls.oldGenArraySize = 0x100000;
  s->controls.summary = FALSE;
  s->cumulativeStatistics.bytesAllocated = 0;
  s->cumulativeStatistics.bytesCopied = 0;
  s->cumulativeStatistics.bytesCopiedMinor = 0;
  s->cumulativeStatistics.bytesHashConsed = 0;
  s->cumulativeStatistics.bytesMarkCompacted = 0;
  s->cumulativeStatistics.markedCards = 0;
  s->cumulativeStatistics.maxBytesLive = 0;
  s->cumulativeStatistics.maxHeapSizeSeen = 0;
  s->cumulativeStatistics.maxStackSizeSeen = 0;
  s->cumulativeStatistics.minorBytesScanned = 0;
  s->cumulativeStatistics.minorBytesSkipped = 0;
  s->cumulativeStatistics.numLimitChecks = 0;
  s->cumulativeStatistics.numCopyingGCs = 0;
  s->cumulativeStatistics.numHashConsGCs = 0;
  s->cumulativeStatistics.numMarkCompactGCs = 0;
  s->cumulativeStatistics.numMinorGCs = 0;
  s->cumulativeStatistics.maxPause = 0;
  rusageZero (&s->cumulativeStatistics.ru_gc);
  rusageZero (&s->cumulativeStatistics.ru_gcCopy);
  rusageZero (&s->cumulativeStatistics.ru_gcMarkCompact);
  rusageZero (&s->cumulativeStatistics.ru_gcMinor);
  s->currentThread = BOGUS_OBJPTR;
  s->hashConsDuringGC = FALSE;
  initHeap (s, &s->heap);
  s->lastMajorStatistics.bytesLive = 0;
  s->lastMajorStatistics.kind = GC_COPYING;
  s->lastMajorStatistics.numMinorGCs = 0;
  s->ratios.copy = 4.0;
  s->ratios.copyGenerational = 4.0;
  s->ratios.grow = 8.0;
  s->ratios.hashCons = 0.0;
  s->ratios.live = 8.0;
  s->ratios.markCompact = 1.04;
  s->ratios.markCompactGenerational = 8.0;
  s->ratios.nursery = 10.0;
  s->ratios.ramSlop = 0.5;
  s->ratios.threadShrink = 0.5;
  s->rusageIsEnabled = FALSE;
  s->savedThread = BOGUS_OBJPTR;
  initHeap (s, &s->secondaryHeap);
  s->signalHandlerThread = BOGUS_OBJPTR;
  s->signalsInfo.amInSignalHandler = FALSE;
  s->signalsInfo.gcSignalHandled = FALSE;
  s->signalsInfo.gcSignalPending = FALSE;
  s->signalsInfo.signalIsPending = FALSE;
  sigemptyset (&s->signalsInfo.signalsHandled);
  sigemptyset (&s->signalsInfo.signalsPending);
  s->startTime = currentTime ();
  // s->sysvals.availRam = ;
  // s->sysvals.totalRam = ;
  // s->sysvals.pageSize = ;
  s->weaks = NULL;

  initSignalStack (s);
  worldFile = NULL;

  unless (isAligned (s->sysvals.pageSize, CARD_SIZE))
    die ("Page size must be a multiple of card size.");
  processAtMLton (s, s->atMLtonsLength, s->atMLtons, &worldFile);
  res = processAtMLton (s, argc, argv, &worldFile);
  if (s->controls.fixedHeap > 0 and s->controls.maxHeap > 0)
    die ("Cannot use both fixed-heap and max-heap.\n");
  unless (ratiosOk (s->ratios))
    die ("invalid ratios");
  // s->totalRam = totalRam (s);
  /* We align s->ram by pageSize so that we can test whether or not we
   * we are using mark-compact by comparing heap size to ram size.  If
   * we didn't round, the size might be slightly off.
   */
  s->sysvals.ram = align ((size_t)(s->ratios.ramSlop * s->sysvals.totalRam), s->sysvals.pageSize);
  if (DEBUG or DEBUG_RESIZING or s->controls.messages)
    fprintf (stderr, "total RAM = %zu  RAM = %zu\n",
             /*uintToCommaString*/(s->sysvals.totalRam),
             /*uintToCommaString*/(s->sysvals.ram));
  if (DEBUG_PROFILE) {
    uint32_t i;
    for (i = 0; i < s->profiling.frameSourcesLength; i++) {
      uint32_t j;
      uint32_t *sourceSeq;
      fprintf (stderr, "%"PRIu32"\n", i);
      sourceSeq = s->profiling.sourceSeqs[s->profiling.frameSources[i]];
      for (j = 1; j <= sourceSeq[0]; j++)
        fprintf (stderr, "\t%s\n",
                 s->profiling.sourceNames[s->profiling.sources[sourceSeq[j]].nameIndex]);
    }
  }
  /* Initialize profiling.  This must occur after processing
   * command-line arguments, because those may just be doing a show
   * prof, in which case we don't want to initialize the atExit.
   */
  if (PROFILE_NONE == s->profiling.kind)
    s->profiling.isOn = FALSE;
  else {
    s->profiling.isOn = TRUE;
    assert (s->profiling.frameSourcesLength == s->frameLayoutsLength);
    switch (s->profiling.kind) {
    case PROFILE_ALLOC:
    case PROFILE_COUNT:
      s->profiling.data = GC_profileNew (s);
      break;
    case PROFILE_NONE:
      die ("impossible PROFILE_NONE");
    case PROFILE_TIME:
      profileTimeInit (s);
      break;
    }
    profileEndState = s;
    atexit (profileEnd);
  }
  if (s->amOriginal) {
    newWorld (s);
    /* The mutator stack invariant doesn't hold,
     * because the mutator has yet to run.
     */
    assert (mutatorInvariant (s, TRUE, FALSE));
  } else {
    loadWorldFromFileName (s, worldFile);
    if (s->profiling.isOn and s->profiling.stack)
      foreachStackFrame (s, enterFrame);
    assert (mutatorInvariant (s, TRUE, TRUE));
  }
  s->amInGC = FALSE;
  return res;
}

/* extern char **environ; /\* for Posix_ProcEnv_environ *\/ */

/* void MLton_init (int argc, char **argv, GC_state s) { */
/*         int start; */

/*         Posix_ProcEnv_environ = (CstringArray)environ; */
/*         start = GC_init (s, argc, argv); */
/*         /\* Setup argv and argc that SML sees. *\/ */
/*         /\* start is now the index of the first real arg. *\/ */
/*         CommandLine_commandName = (uint)(argv[0]); */
/*         CommandLine_argc = argc - start; */
/*         CommandLine_argv = (uint)(argv + start); */
/* } */
