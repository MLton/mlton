/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * All ML objects (including ML execution stacks) are allocated in a
 * contiguous heap.  The heap has the following general layout:
 * 
 *  ----------------------------------------------------
 *  |    old generation    |               |  nursery  |
 *  ----------------------------------------------------
 *  |------oldGenSize------|
 *  |-----------------------size-----------------------|
 *  ^                                      ^
 *  start                                  nursery
*/

typedef struct GC_heap {
  pointer nursery; /* start of nursery */
  size_t oldGenSize; /* size of old generation */
  size_t size; /* size of heap */
  pointer start; /* start of heap (and old generation) */
} *GC_heap;

#define GC_HEAP_LIMIT_SLOP 512

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isPointerInHeap (GC_state s, pointer p);
static inline bool isPointerInOldGen (GC_state s, pointer p);
static inline bool isPointerInNursery (GC_state s, pointer p);
static inline bool isPointerInFromSpace (GC_state s, pointer p);
static inline bool isObjptrInHeap (GC_state s, objptr op);
static inline bool isObjptrInOldGen (GC_state s, objptr op);
static inline bool isObjptrInNursery (GC_state s, objptr op);
static inline bool isObjptrInFromSpace (GC_state s, objptr op);
static inline bool hasHeapBytesFree (GC_state s, size_t oldGen, size_t nursery);
static inline bool isHeapInit (GC_heap h);

static void displayHeap (GC_state s, GC_heap heap, FILE *stream);

static inline void initHeap (GC_state s, GC_heap h);
static inline size_t sizeofHeapDesired (GC_state s, size_t live, size_t currentSize);

static inline void releaseHeap (GC_state s, GC_heap h);
static void shrinkHeap (GC_state s, GC_heap h, size_t keep);
static bool createHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize);
static bool createHeapSecondary (GC_state s, size_t desiredSize);
static bool remapHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize);
static void growHeap (GC_state s, size_t desiredSize, size_t minSize);
static void resizeHeap (GC_state s, size_t minSize);
static void resizeHeapSecondary (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
