/* Copyright (C) 2012,2014 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_state {
  /* These fields are at the front because they are the most commonly
   * referenced, and having them at smaller offsets may decrease code
   * size and improve cache performance.
   */
  pointer frontier; /* heap.start <= frontier < limit */
  pointer limit; /* limit = heap.start + heap.size */
  pointer stackTop; /* Top of stack in current thread. */
  pointer stackLimit; /* stackBottom + stackSize - maxFrameSize */
  size_t exnStack;
  /* Alphabetized fields follow. */
  size_t alignment; /* */
  bool amInGC;
  bool amOriginal;
  char **atMLtons; /* Initial @MLton args, processed before command line. */
  int atMLtonsLength;
  uint32_t atomicState;
  objptr callFromCHandlerThread; /* Handler for exported C calls (in heap). */
  struct GC_callStackState callStackState;
  bool canMinor; /* TRUE iff there is space for a minor gc. */
  struct GC_controls controls;
  CopyObjectMap* copyObjectMap;
  struct GC_cumulativeStatistics cumulativeStatistics;
  objptr currentThread; /* Currently executing thread (in heap). */
  struct GC_forwardState forwardState;
  GC_frameLayout frameLayouts; /* Array of frame layouts. */
  uint32_t frameLayoutsLength; /* Cardinality of frameLayouts array. */
  struct GC_generationalMaps generationalMaps;
  objptr *globals;
  uint32_t globalsLength;
  bool hashConsDuringGC;
  struct GC_heap heap;
  struct GC_lastMajorStatistics lastMajorStatistics;
  pointer limitPlusSlop; /* limit + GC_HEAP_LIMIT_SLOP */
  int (*loadGlobals)(FILE *f); /* loads the globals from the file. */
  uint32_t magic; /* The magic number for this executable. */
  uint32_t maxFrameSize;
  bool mutatorMarksCards;
  GC_objectHashTable objectHashTable;
  GC_objectType objectTypes; /* Array of object types. */
  uint32_t objectTypesLength; /* Cardinality of objectTypes array. */
  struct GC_profiling profiling;
  GC_frameIndex (*returnAddressToFrameIndex) (GC_returnAddress ra);
  objptr savedThread; /* Result of GC_copyCurrentThread.
                       * Thread interrupted by arrival of signal.
                       */
  int (*saveGlobals)(FILE *f); /* saves the globals to the file. */
  bool saveWorldStatus; /* */
  struct GC_heap secondaryHeap; /* Used for major copying collection. */
  objptr signalHandlerThread; /* Handler for signals (in heap). */
  struct GC_signalsInfo signalsInfo;
  struct GC_sourceMaps sourceMaps;
  pointer stackBottom; /* Bottom of stack in current thread. */
  struct GC_sysvals sysvals;
  struct GC_translateState translateState;
  struct GC_vectorInit *vectorInits;
  uint32_t vectorInitsLength;
  GC_weak weaks; /* Linked list of (live) weak pointers */
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayGCState (GC_state s, FILE *stream);

static inline size_t sizeofGCStateCurrentStackUsed (GC_state s);
static inline void setGCStateCurrentThreadAndStack (GC_state s);
static void setGCStateCurrentHeap (GC_state s,
                                   size_t oldGenBytesRequested,
                                   size_t nurseryBytesRequested);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE bool GC_getAmOriginal (GC_state s);
PRIVATE void GC_setAmOriginal (GC_state s, bool b);
PRIVATE void GC_setControlsMessages (GC_state s, bool b);
PRIVATE void GC_setControlsSummary (GC_state s, bool b);
PRIVATE void GC_setControlsRusageMeasureGC (GC_state s, bool b);
PRIVATE uintmax_t GC_getCumulativeStatisticsBytesAllocated (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMinorGCs (GC_state s);
PRIVATE size_t GC_getCumulativeStatisticsMaxBytesLive (GC_state s);
PRIVATE void GC_setHashConsDuringGC (GC_state s, bool b);
PRIVATE size_t GC_getLastMajorStatisticsBytesLive (GC_state s);

PRIVATE pointer GC_getCallFromCHandlerThread (GC_state s);
PRIVATE void GC_setCallFromCHandlerThread (GC_state s, pointer p);
PRIVATE pointer GC_getCurrentThread (GC_state s);
PRIVATE pointer GC_getSavedThread (GC_state s);
PRIVATE void GC_setSavedThread (GC_state s, pointer p);
PRIVATE void GC_setSignalHandlerThread (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

PRIVATE struct rusage* GC_getRusageGCAddr (GC_state s);

PRIVATE sigset_t* GC_getSignalsHandledAddr (GC_state s);
PRIVATE sigset_t* GC_getSignalsPendingAddr (GC_state s);
PRIVATE void GC_setGCSignalHandled (GC_state s, bool b);
PRIVATE bool GC_getGCSignalPending (GC_state s);
PRIVATE void GC_setGCSignalPending (GC_state s, bool b);
