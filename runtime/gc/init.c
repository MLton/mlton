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
      WORD_SIZE // for the sign
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
/*         struct GC_intInfInit *inits; */
/*         pointer frontier; */
/*         char    *str; */
/*         uint    slen, */
/*                 llen, */
/*                 alen, */
/*                 i, */
/*                 index; */
/*         bool    neg, */
/*                 hex; */
/*         bignum  *bp; */
/*         uchar   *cp; */

/*         assert (isAlignedFrontier (s, s->frontier)); */
/*         frontier = s->frontier; */
/*         for (index = 0; index < s->intInfInitsSize; ++index) { */
/*                 inits = &s->intInfInits[index]; */
/*                 str = inits->mlstr; */
/*                 assert (inits->globalIndex < s->globalsSize); */
/*                 neg = *str == '~'; */
/*                 if (neg) */
/*                         ++str; */
/*                 slen = strlen (str); */
/*                 hex = str[0] == '0' && str[1] == 'x'; */
/*                 if (hex) { */
/*                         str += 2; */
/*                         slen -= 2; */
/*                         llen = (slen + 7) / 8; */
/*                 } else */
/*                         llen = (slen + 8) / 9; */
/*                 assert (slen > 0); */
/*                 bp = (bignum *)frontier; */
/*                 cp = (uchar *)&bp->limbs[llen]; */
/*                 for (i = 0; i != slen; ++i) */
/*                         if ('0' <= str[i] && str[i] <= '9') */
/*                                 cp[i] = str[i] - '0' + 0; */
/*                         else if ('a' <= str[i] && str[i] <= 'f') */
/*                                 cp[i] = str[i] - 'a' + 0xa; */
/*                         else { */
/*                                 assert('A' <= str[i] && str[i] <= 'F'); */
/*                                 cp[i] = str[i] - 'A' + 0xA; */
/*                         } */
/*                 alen = mpn_set_str (bp->limbs, cp, slen, hex ? 0x10 : 10); */
/*                 assert (alen <= llen); */
/*                 if (alen <= 1) { */
/*                         uint    val, */
/*                                 ans; */

/*                         if (alen == 0) */
/*                                 val = 0; */
/*                         else */
/*                                 val = bp->limbs[0]; */
/*                         if (neg) { */
/*                                 /\* */
/*                                  * We only fit if val in [1, 2^30]. */
/*                                  *\/ */
/*                                 ans = - val; */
/*                                 val = val - 1; */
/*                         } else */
/*                                 /\* */
/*                                  * We only fit if val in [0, 2^30 - 1]. */
/*                                  *\/ */
/*                                 ans = val; */
/*                         if (val < (uint)1<<30) { */
/*                                 s->globals[inits->globalIndex] =  */
/*                                         (pointer)(ans<<1 | 1); */
/*                                 continue; */
/*                         } */
/*                 } */
/*                 s->globals[inits->globalIndex] = (pointer)&bp->isneg; */
/*                 bp->counter = 0; */
/*                 bp->card = alen + 1; */
/*                 bp->magic = BIGMAGIC; */
/*                 bp->isneg = neg; */
/*                 frontier = alignFrontier (s, (pointer)&bp->limbs[alen]); */
/*         } */
/*         assert (isAlignedFrontier (s, frontier)); */
/*         s->frontier = frontier; */
/*         GC_profileAllocInc (s, frontier - s->frontier); */
/*         s->bytesAllocated += frontier - s->frontier; */
}

static void initVectors (GC_state s) {
  struct GC_vectorInit *inits;
  pointer frontier;
  uint32_t i;

  assert (isAlignedFrontier (s, s->frontier));
  inits = s->vectorInits;
  frontier = s->frontier;
  for (i = 0; i < s->vectorInitsLength; ++i) {
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
    *((GC_header*)(frontier)) = objectHeader (typeIndex);
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
  heapCreate (s, &s->heap, 
              heapDesiredSize (s, s->lastMajorStatistics.bytesLive, 0),
              s->lastMajorStatistics.bytesLive);
  createCardMapAndCrossMap (s);
  start = alignFrontier (s, s->heap.start);
  s->frontier = start;
  initIntInfs (s);
  initVectors (s);
  assert ((size_t)(s->frontier - start) <= s->lastMajorStatistics.bytesLive);
  s->heap.oldGenSize = s->frontier - s->heap.start;
  heapSetNursery (s, 0, 0);
  thread = newThread (s, initialStackSize (s));
  switchToThread (s, pointerToObjptr((pointer)thread, s->heap.start));
}

/* /\* worldTerminator is used to separate the human readable messages at the  */
/*  * beginning of the world file from the machine readable data. */
/*  *\/ */
/* static const char worldTerminator = '\000'; */

/* static void loadWorld (GC_state s, char *fileName) { */
/*         FILE *file; */
/*         uint magic; */
/*         pointer oldGen; */
/*         int c; */
        
/*         if (DEBUG_WORLD) */
/*                 fprintf (stderr, "loadWorld (%s)\n", fileName); */
/*         file = sfopen (fileName, "rb"); */
/*         until ((c = fgetc (file)) == worldTerminator or EOF == c); */
/*         if (EOF == c) die ("Invalid world."); */
/*         magic = sfreadUint (file); */
/*         unless (s->magic == magic) */
/*                 die ("Invalid world: wrong magic number."); */
/*         oldGen = (pointer) sfreadUint (file); */
/*         s->oldGenSize = sfreadUint (file); */
/*         s->callFromCHandler = (GC_thread) sfreadUint (file); */
/*         s->canHandle = sfreadUint (file); */
/*         s->currentThread = (GC_thread) sfreadUint (file); */
/*         s->signalHandler = (GC_thread) sfreadUint (file); */
/*         heapCreate (s, &s->heap, heapDesiredSize (s, s->oldGenSize, 0), */
/*                         s->oldGenSize); */
/*         createCardMapAndCrossMap (s); */
/*         sfread (s->heap.start, 1, s->oldGenSize, file); */
/*         (*s->loadGlobals) (file); */
/*         unless (EOF == fgetc (file)) */
/*                 die ("Invalid world: junk at end of file."); */
/*         fclose (file); */
/*         /\* translateHeap must occur after loading the heap and globals, since it */
/*          * changes pointers in all of them. */
/*          *\/ */
/*         translateHeap (s, oldGen, s->heap.start, s->oldGenSize); */
/*         setNursery (s, 0, 0); */
/*         setStack (s); */
/* } */

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
          ++i;
          if (i == argc)
            die ("@MLton copy-ratio missing argument.");
          s->ratios.copy = stringToFloat (argv[i++]);
        } else if (0 == strcmp(arg, "fixed-heap")) {
          ++i;
          if (i == argc)
            die ("@MLton fixed-heap missing argument.");
          s->controls.fixedHeap = align (stringToBytes (argv[i++]),
                                         2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "gc-messages")) {
          ++i;
          s->controls.messages = TRUE;
        } else if (0 == strcmp (arg, "gc-summary")) {
          ++i;
#if (defined (__MINGW32__))
          fprintf (stderr, "Warning: MinGW doesn't yet support gc-summary\n");
#else
          s->controls.summary = TRUE;
#endif
        } else if (0 == strcmp (arg, "copy-generational-ratio")) {
          ++i;
          if (i == argc)
            die ("@MLton copy-generational-ratio missing argument.");
          s->ratios.copyGenerational = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "grow-ratio")) {
          ++i;
          if (i == argc)
            die ("@MLton grow-ratio missing argument.");
          s->ratios.grow = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "hash-cons")) {
          ++i;
          if (i == argc)
            die ("@MLton hash-cons missing argument.");
          s->ratios.hashCons = stringToFloat (argv[i++]);
          unless (0.0 <= s->ratios.hashCons
                  and s->ratios.hashCons <= 1.0)
            die ("@MLton hash-cons argument must be between 0.0 and 1.0");
        } else if (0 == strcmp (arg, "live-ratio")) {
          ++i;
          if (i == argc)
            die ("@MLton live-ratio missing argument.");
          s->ratios.live = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "load-world")) {
          unless (s->controls.mayLoadWorld)
            die ("May not load world.");
          ++i;
          s->amOriginal = FALSE;
          if (i == argc)
            die ("@MLton load-world missing argument.");
          *worldFile = argv[i++];
        } else if (0 == strcmp (arg, "max-heap")) {
          ++i;
          if (i == argc)
            die ("@MLton max-heap missing argument.");
          s->controls.maxHeap = align (stringToBytes (argv[i++]),
                                       2 * s->sysvals.pageSize);
        } else if (0 == strcmp (arg, "mark-compact-generational-ratio")) {
          ++i;
          if (i == argc)
            die ("@MLton mark-compact-generational-ratio missing argument.");
          s->ratios.markCompactGenerational = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "mark-compact-ratio")) {
          ++i;
          if (i == argc)
            die ("@MLton mark-compact-ratio missing argument.");
          s->ratios.markCompact = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "no-load-world")) {
          ++i;
          s->controls.mayLoadWorld = FALSE;
        } else if (0 == strcmp (arg, "nursery-ratio")) {
          ++i;
          if (i == argc)
            die ("@MLton nursery-ratio missing argument.");
          s->ratios.nursery = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "ram-slop")) {
          ++i;
          if (i == argc)
            die ("@MLton ram-slop missing argument.");
          s->ratios.ramSlop = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "show-prof")) {
          showProf (s);
          exit (0);
        } else if (0 == strcmp (arg, "stop")) {
          ++i;
          s->controls.mayProcessAtMLton = FALSE;
        } else if (0 == strcmp (arg, "thread-shrink-ratio")) {
          ++i;
          if (i == argc)
            die ("@MLton thread-shrink-ratio missing argument.");
          s->ratios.threadShrink = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "use-mmap")) {
          ++i;
          MLton_Platform_CygwinUseMmap = TRUE;
        } else if (0 == strcmp (arg, "--")) {
          ++i;
          done = TRUE;
        } else if (i > 1)
          die ("Strange @MLton arg: %s", argv[i]);
        else done = TRUE;
      }
    }
  }
  return i;
}

/* int GC_init (GC_state s, int argc, char **argv) { */
/*         char *worldFile; */
/*         int i; */

/*         assert (isAligned (sizeof (struct GC_stack), s->alignment)); */
/*         assert (isAligned (GC_NORMAL_HEADER_SIZE + sizeof (struct GC_thread), */
/*                                 s->alignment)); */
/*         assert (isAligned (GC_NORMAL_HEADER_SIZE + sizeof (struct GC_weak), */
/*                                 s->alignment)); */
/*         MLton_Platform_CygwinUseMmap = FALSE; */
/*         s->amInGC = TRUE; */
/*         s->amInMinorGC = FALSE; */
/*         s->bytesAllocated = 0; */
/*         s->bytesCopied = 0; */
/*         s->bytesCopiedMinor = 0; */
/*         s->bytesMarkCompacted = 0; */
/*         s->callFromCHandler = BOGUS_THREAD; */
/*         s->canHandle = 0; */
/*         s->cardSize = 0x1 << CARD_SIZE_LOG2; */
/*         s->copyRatio = 4.0; */
/*         s->copyGenerationalRatio = 4.0; */
/*         s->currentThread = BOGUS_THREAD; */
/*         s->fixedHeap = 0.0; */
/*         s->gcSignalIsPending = FALSE; */
/*         s->growRatio = 8.0; */
/*         s->handleGCSignal = FALSE; */
/*         s->hashConsDuringGC = FALSE; */
/*         s->hashConsFrequency = 0.0; */
/*         s->inSignalHandler = FALSE; */
/*         s->isOriginal = TRUE; */
/*         s->lastMajor = GC_COPYING; */
/*         s->liveRatio = 8.0; */
/*         s->markCompactRatio = 1.04; */
/*         s->markCompactGenerationalRatio = 8.0; */
/*         s->markedCards = 0; */
/*         s->maxBytesLive = 0; */
/*         s->maxHeap = 0; */
/*         s->maxHeapSizeSeen = 0; */
/*         s->maxPause = 0; */
/*         s->maxStackSizeSeen = 0; */
/*         s->mayLoadWorld = TRUE; */
/*         s->mayProcessAtMLton = TRUE; */
/*         s->messages = FALSE; */
/*         s->minorBytesScanned = 0; */
/*         s->minorBytesSkipped = 0; */
/*         s->numCopyingGCs = 0; */
/*         s->numLCs = 0; */
/*         s->numHashConsGCs = 0; */
/*         s->numMarkCompactGCs = 0; */
/*         s->numMinorGCs = 0; */
/*         s->numMinorsSinceLastMajor = 0; */
/*         s->nurseryRatio = 10.0; */
/*         s->oldGenArraySize = 0x100000; */
/*         s->pageSize = getpagesize (); */
/*         s->ramSlop = 0.5; */
/*         s->rusageIsEnabled = FALSE; */
/*         s->savedThread = BOGUS_THREAD; */
/*         s->signalHandler = BOGUS_THREAD; */
/*         s->signalIsPending = FALSE; */
/*         s->startTime = currentTime (); */
/*         s->summary = FALSE; */
/*         s->threadShrinkRatio = 0.5; */
/*         s->weaks = NULL; */
/*         heapInit (&s->heap); */
/*         heapInit (&s->heap2); */
/*         sigemptyset (&s->signalsHandled); */
/*         initSignalStack (s); */
/*         sigemptyset (&s->signalsPending); */
/*         rusageZero (&s->ru_gc); */
/*         rusageZero (&s->ru_gcCopy); */
/*         rusageZero (&s->ru_gcMarkCompact); */
/*         rusageZero (&s->ru_gcMinor); */
/*         worldFile = NULL; */
/*         unless (isAligned (s->pageSize, s->cardSize)) */
/*                 die ("Page size must be a multiple of card size."); */
/*         processAtMLton (s, s->atMLtonsSize, s->atMLtons, &worldFile); */
/*         i = processAtMLton (s, argc, argv, &worldFile); */
/*         if (s->fixedHeap > 0 and s->maxHeap > 0) */
/*                 die ("Cannot use both fixed-heap and max-heap.\n"); */
/*         unless (ratiosOk (s)) */
/*                 die ("invalid ratios"); */
/*         s->totalRam = totalRam (s); */
/*         /\* We align s->ram by pageSize so that we can test whether or not we */
/*          * we are using mark-compact by comparing heap size to ram size.  If  */
/*          * we didn't round, the size might be slightly off. */
/*          *\/ */
/*         s->ram = align (s->ramSlop * s->totalRam, s->pageSize); */
/*         if (DEBUG or DEBUG_RESIZING or s->messages) */
/*                 fprintf (stderr, "total RAM = %s  RAM = %s\n", */
/*                                 uintToCommaString (s->totalRam),  */
/*                                 uintToCommaString (s->ram)); */
/*         if (DEBUG_PROFILE) { */
/*                 int i; */
/*                         for (i = 0; i < s->frameSourcesSize; ++i) { */
/*                         int j; */
/*                         uint *sourceSeq; */
/*                                 fprintf (stderr, "%d\n", i); */
/*                         sourceSeq = s->sourceSeqs[s->frameSources[i]]; */
/*                         for (j = 1; j <= sourceSeq[0]; ++j) */
/*                                 fprintf (stderr, "\t%s\n", */
/*                                                 s->sourceNames[s->sources[sourceSeq[j]].nameIndex]); */
/*                 } */
/*         } */
/*         /\* Initialize profiling.  This must occur after processing command-line  */
/*          * arguments, because those may just be doing a show prof, in which  */
/*          * case we don't want to initialize the atExit. */
/*          *\/ */
/*         if (PROFILE_NONE == s->profileKind) */
/*                 s->profilingIsOn = FALSE; */
/*         else { */
/*                 s->profilingIsOn = TRUE; */
/*                 assert (s->frameSourcesSize == s->frameLayoutsSize); */
/*                 switch (s->profileKind) { */
/*                 case PROFILE_ALLOC: */
/*                 case PROFILE_COUNT: */
/*                         s->profile = GC_profileNew (s); */
/*                 break; */
/*                 case PROFILE_NONE: */
/*                         die ("impossible PROFILE_NONE"); */
/*                 case PROFILE_TIME: */
/*                         profileTimeInit (s); */
/*                 break; */
/*                 } */
/*                 profileEndState = s; */
/*                 atexit (profileEnd); */
/*         } */
/*         if (s->isOriginal) { */
/*                 newWorld (s); */
/*                 /\* The mutator stack invariant doesn't hold, */
/*                  * because the mutator has yet to run. */
/*                  *\/ */
/*                 assert (mutatorInvariant (s, TRUE, FALSE)); */
/*         } else { */
/*                 loadWorld (s, worldFile); */
/*                 if (s->profilingIsOn and s->profileStack) */
/*                         GC_foreachStackFrame (s, enterFrame); */
/*                 assert (mutatorInvariant (s, TRUE, TRUE)); */
/*         } */
/*         s->amInGC = FALSE; */
/*         return i; */
/* } */

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

/* static void displayCol (FILE *out, int width, string s) { */
/*         int extra; */
/*         int i; */
/*         int len; */

/*         len = strlen (s); */
/*         if (len < width) { */
/*                 extra = width - len; */
/*                 for (i = 0; i < extra; ++i) */
/*                         fprintf (out, " "); */
/*         } */
/*         fprintf (out, "%s\t", s); */
/* } */

/* static void displayCollectionStats (FILE *out, string name, struct rusage *ru,  */
/*                                         uint num, ullong bytes) { */
/*         uint ms; */

/*         ms = rusageTime (ru); */
/*         fprintf (out, "%s", name); */
/*         displayCol (out, 7, uintToCommaString (ms)); */
/*         displayCol (out, 7, uintToCommaString (num)); */
/*         displayCol (out, 15, ullongToCommaString (bytes)); */
/*         displayCol (out, 15,  */
/*                         (ms > 0) */
/*                         ? uintToCommaString (1000.0 * (float)bytes/(float)ms) */
/*                         : "-"); */
/*         fprintf (out, "\n"); */
/* } */

/* void GC_done (GC_state s) { */
/*         FILE *out; */

/*         enter (s); */
/*         minorGC (s); */
/*         out = stderr; */
/*         if (s->summary) { */
/*                 double time; */
/*                 uint gcTime; */

/*                 gcTime = rusageTime (&s->ru_gc); */
/*                 fprintf (out, "GC type\t\ttime ms\t number\t\t  bytes\t      bytes/sec\n"); */
/*                 fprintf (out, "-------------\t-------\t-------\t---------------\t---------------\n"); */
/*                 displayCollectionStats */
/*                         (out, "copying\t\t", &s->ru_gcCopy, s->numCopyingGCs,  */
/*                                 s->bytesCopied); */
/*                 displayCollectionStats */
/*                         (out, "mark-compact\t", &s->ru_gcMarkCompact,  */
/*                                 s->numMarkCompactGCs, s->bytesMarkCompacted); */
/*                 displayCollectionStats */
/*                         (out, "minor\t\t", &s->ru_gcMinor, s->numMinorGCs,  */
/*                                 s->bytesCopiedMinor); */
/*                 time = (double)(currentTime () - s->startTime); */
/*                 fprintf (out, "total GC time: %s ms (%.1f%%)\n", */
/*                                 intToCommaString (gcTime),  */
/*                                 (0.0 == time)  */
/*                                         ? 0.0  */
/*                                         : 100.0 * ((double) gcTime) / time); */
/*                 fprintf (out, "max pause: %s ms\n", */
/*                                 uintToCommaString (s->maxPause)); */
/*                 fprintf (out, "total allocated: %s bytes\n", */
/*                                 ullongToCommaString (s->bytesAllocated)); */
/*                 fprintf (out, "max live: %s bytes\n", */
/*                                 uintToCommaString (s->maxBytesLive)); */
/*                 fprintf (out, "max semispace: %s bytes\n",  */
/*                                 uintToCommaString (s->maxHeapSizeSeen)); */
/*                 fprintf (out, "max stack size: %s bytes\n",  */
/*                                 uintToCommaString (s->maxStackSizeSeen)); */
/*                 fprintf (out, "marked cards: %s\n",  */
/*                                 ullongToCommaString (s->markedCards)); */
/*                 fprintf (out, "minor scanned: %s bytes\n", */
/*                                 uintToCommaString (s->minorBytesScanned)); */
/*                 fprintf (out, "minor skipped: %s bytes\n",  */
/*                                 uintToCommaString (s->minorBytesSkipped)); */
/*         } */
/*         heapRelease (s, &s->heap); */
/*         heapRelease (s, &s->heap2); */
/* } */
