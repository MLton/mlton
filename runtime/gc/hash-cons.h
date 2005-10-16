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
#define GC_HASH_SIZE sizeof(GC_hash)
#define PRIxHASH PRIx32
#define FMTHASH "0x%08"PRIxHASH

typedef struct GC_objectHashElement {
  GC_hash hash;
  pointer object;
} *GC_objectHashElement;

typedef struct GC_objectHashTable {
  struct GC_objectHashElement *elements;
  bool elementsIsInHeap;
  uint32_t elementsLengthCur;
  uint32_t elementsLengthMax;
  uint32_t elementsLengthMaxLog2;
  bool mayInsert;
} *GC_objectHashTable;

/*
void maybeShareObjptr (GC_state s,
                       objptr *opp,
                       bool shouldHashCons);
*/
