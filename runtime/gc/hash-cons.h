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

GC_objectHashTable allocHashTable (GC_state s);
void freeHashTable (GC_objectHashTable t);

pointer insertHashTableElem (GC_state s, 
                             GC_objectHashTable t, GC_hash hash, 
                             pointer object, pointer max, bool mightBeThere);
void growHashTableMaybe (GC_state s, GC_objectHashTable t);

pointer hashConsPointer (GC_state s, pointer object, bool countBytesHashConsed);
void shareObjptr (GC_state s, objptr *opp);

void printBytesHashConsedMessage (GC_state s, uintmax_t total);
