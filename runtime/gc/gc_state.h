
typedef struct GC_state {
  struct GC_heap heap;
  struct GC_heap secondaryHeap; /* Used for major copying collection. */
  GC_objectType *objectTypes; /* Array of object types. */
  uint32_t objectTypesSize; /* Cardinality of objectTypes array. */
} *GC_state;
