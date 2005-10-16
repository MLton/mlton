
typedef struct GC_state {
  size_t alignment; /* */
  bool amInGC;
  bool amInMinorGC;
  uint32_t atomicState;
  objptr callFromCHandlerThread; /* Handler for exported C calls (in heap). */
  bool canMinor; /* TRUE iff there is space for a minor gc. */
  struct GC_controls controls;
  struct GC_cumulativeStatistics cumulativeStatistics;
  objptr currentThread; /* Currently executing thread (in heap). */
  uint32_t exnStack;
  GC_frameLayout *frameLayouts; /* Array of frame layouts. */
  uint32_t frameLayoutsLength; /* Cardinality of frameLayouts array. */
  pointer frontier; /* heap.start <= frontier < limit */
  struct GC_generationalMaps generationalMaps;
  objptr *globals;
  uint32_t globalsLength;
  /*Bool*/bool hashConsDuringGC;
  struct GC_heap heap;
  struct GC_lastMajorStatistics lastMajorStatistics;
  pointer limit; /* limit = heap.start + heap.totalBytes */
  pointer limitPlusSlop; /* limit + LIMIT_SLOP */
  uint32_t maxFrameSize;
  /*Bool*/bool mutatorMarksCards;
  GC_objectHashTable objectHashTable;
  GC_objectType *objectTypes; /* Array of object types. */
  uint32_t objectTypesLength; /* Cardinality of objectTypes array. */
  uint32_t (*returnAddressToFrameIndex) (GC_returnAddress ra);
  struct GC_ratios ratios;
  objptr savedThread; /* Result of GC_copyCurrentThread.
                       * Thread interrupted by arrival of signal.
                       */
  struct GC_heap secondaryHeap; /* Used for major copying collection. */
  objptr signalHandlerThread; /* Handler for signals (in heap). */
  /*Bool*/bool signalIsPending;
  pointer stackBottom; /* Bottom of stack in current thread. */
  pointer stackLimit; /* stackBottom + stackSize - maxFrameSize */
  pointer stackTop; /* Top of stack in current thread. */
  struct GC_sysvals sysvals;
  GC_weak weaks; /* Linked list of (live) weak pointers */
} *GC_state;
