
typedef struct GC_state {
  size_t alignment; /* */
  bool amInGC;
  bool amInMinorGC;
  objptr callFromCHandler; /* Handler for exported C calls (in heap). */
  bool canMinor; /* TRUE iff there is space for a minor gc. */
  struct GC_cumulativeStatistics cumulative;
  objptr currentThread; /* Currently executing thread (in heap). */
  GC_frameLayout *frameLayouts; /* Array of frame layouts. */
  uint32_t frameLayoutsLength; /* Cardinality of frameLayouts array. */
  pointer frontier; /* heap.start <= frontier < limit */
  struct GC_generationalMaps generational;
  objptr *globals;
  uint32_t globalsLength;
  struct GC_heap heap;
  struct GC_lastMajorStatistics lastMajor;
  pointer limit; /* limit = heap.start + heap.totalBytes */
  pointer limitPlusSlop; /* limit + LIMIT_SLOP */
  uint32_t maxFrameSize;
  /*Bool*/bool mutatorMarksCards;
  GC_objectType *objectTypes; /* Array of object types. */
  uint32_t objectTypesLength; /* Cardinality of objectTypes array. */
  size_t pageSize;
  uint32_t (*returnAddressToFrameIndex) (GC_returnAddress ra);
  objptr savedThread; /* Result of GC_copyCurrentThread.
                       * Thread interrupted by arrival of signal.
                       */
  struct GC_heap secondaryHeap; /* Used for major copying collection. */
  objptr signalHandler; /* Handler for signals (in heap). */
  pointer stackBottom; /* Bottom of stack in current thread. */
  pointer stackTop; /* Top of stack in current thread. */
  /*Bool*/bool summary; /* Print a summary of gc info when program exits. */
  /*Bool*/bool messages; /* Print a message at the start and end of each gc. */
  float threadShrinkRatio;
  GC_weak weaks; /* Linked list of (live) weak pointers */
} *GC_state;
