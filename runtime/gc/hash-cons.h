/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                       Object hash consing                        */
/* ---------------------------------------------------------------- */

typedef uint32_t GC_hash;

typedef struct GC_objectHashElement {
  GC_hash hash;
  pointer object;
} *GC_objectHashElement;

typedef struct GC_objectHashTable {
  struct GC_objectHashElement *elements;
  bool elementsIsInHeap;
  size_t elementsSize;
  int log2ElementsSize;
  bool mayInsert;
  int32_t numElements;
} *GC_ObjectHashTable;

pointer hashCons (GC_state s, pointer object,
                  bool countBytesHashConsed);
void maybeShareObjptr (GC_state s,
                       objptr *opp,
                       bool shouldHashCons);
