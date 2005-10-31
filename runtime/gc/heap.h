/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

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
  size_t oldGenSize; /* size of old generation */
  pointer nursery; /* start of nursery */
  size_t size; /* size of heap */
  pointer start; /* start of heap (and old generation) */
} *GC_heap;

#define GC_HEAP_LIMIT_SLOP 512

bool isPointerInHeap (GC_state s, pointer p);
bool isPointerInOldGen (GC_state s, pointer p);
bool isPointerInNursery (GC_state s, pointer p);
bool isPointerInFromSpace (GC_state s, pointer p);
bool isObjptrInHeap (GC_state s, objptr op);
bool isObjptrInOldGen (GC_state s, objptr op);
bool isObjptrInNursery (GC_state s, objptr op);
bool isObjptrInFromSpace (GC_state s, objptr op);
bool hasHeapBytesFree (GC_state s, size_t oldGen, size_t nursery);
bool isHeapInit (GC_heap h);

void displayHeap (GC_state s, GC_heap heap, FILE *stream);
void initHeap (GC_state s, GC_heap h);
size_t sizeofHeapDesired (GC_state s, size_t live, size_t currentSize);

void releaseHeap (GC_state s, GC_heap h);
void shrinkHeap (GC_state s, GC_heap h, size_t keep);
bool createHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize);
bool createHeapSecondary (GC_state s, size_t desiredSize);
bool remapHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize);
void growHeap (GC_state s, size_t desiredSize, size_t minSize);
void resizeHeap (GC_state s, size_t minSize);
void resizeHeapSecondary (GC_state s);

void setHeapNursery (GC_state s, 
                     size_t oldGenBytesRequested, 
                     size_t nurseryBytesRequested);
