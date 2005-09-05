
typedef struct GC_state {
  size_t alignment; /* */
  objptr callFromCHandler; /* Handler for exported C calls (in heap). */
  objptr currentThread; /* Currently executing thread (in heap). */
  objptr *globals;
  uint32_t globalsSize;
  struct GC_heap heap;
  GC_objectType *objectTypes; /* Array of object types. */
  uint32_t objectTypesSize; /* Cardinality of objectTypes array. */
  objptr savedThread; /* Result of GC_copyCurrentThread.
                       * Thread interrupted by arrival of signal.
                       */
  struct GC_heap secondaryHeap; /* Used for major copying collection. */
  objptr signalHandler; /* Handler for signals (in heap). */
  /*Bool*/bool summary; /* Print a summary of gc info when program exits. */
  GC_weak weaks; /* Linked list of (live) weak pointers */
} *GC_state;
