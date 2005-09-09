
typedef struct GC_state {
  size_t alignment; /* */
  bool amInGC;
  bool amInMinorGC;
  objptr callFromCHandler; /* Handler for exported C calls (in heap). */
  struct GC_cumulativeStatistics cumulative;
  objptr currentThread; /* Currently executing thread (in heap). */
  GC_frameLayout *frameLayouts; /* Array of frame layouts. */
  uint32_t frameLayoutsSize; /* Cardinality of frameLayouts array. */
  pointer frontier; /* heap.start <= frontier < limit */
  objptr *globals;
  uint32_t globalsSize;
  struct GC_heap heap;
  struct GC_lastMajorStatistics lastMajor;
  pointer limit; /* limit = heap.start + heap.totalBytes */
  uint32_t maxFrameSize;
  GC_objectType *objectTypes; /* Array of object types. */
  uint32_t objectTypesSize; /* Cardinality of objectTypes array. */
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
