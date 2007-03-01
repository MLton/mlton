/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Array objects have the following layout:
 * 
 * counter word32 :: 
 * length word32 :: 
 * header word32 :: 
 * ( (non heap-pointers)* :: (heap pointers)* )*
 *
 * The counter word is used by mark compact GC.  The length word is
 * the number of elements in the array.  Array elements have the same
 * individual layout as normal objects, omitting the header word.
 */
#ifdef GC_MODEL_NATIVE32
typedef uint32_t GC_arrayLength;
#define GC_ARRAY_LENGTH_SIZE sizeof(GC_arrayLength)
#define PRIxARRLEN PRIu32
#define FMTARRLEN "%"PRIxARRLEN
typedef GC_arrayLength GC_arrayCounter;
#define GC_ARRAY_COUNTER_SIZE sizeof(GC_arrayCounter)
#define PRIxARRCTR PRIxARRLEN
#define FMTARRCTR "%"PRIxARRCTR
#define GC_ARRAY_HEADER_SIZE (GC_ARRAY_COUNTER_SIZE + GC_ARRAY_LENGTH_SIZE + GC_HEADER_SIZE)
#endif
#ifdef GC_MODEL_NATIVE64
typedef uint64_t GC_arrayLength;
#define GC_ARRAY_LENGTH_SIZE sizeof(GC_arrayLength)
#define PRIxARRLEN PRIu64
#define FMTARRLEN "%"PRIxARRLEN
typedef GC_arrayLength GC_arrayCounter;
#define GC_ARRAY_COUNTER_SIZE sizeof(GC_arrayCounter)
#define PRIxARRCTR PRIxARRLEN
#define FMTARRCTR "%"PRIxARRCTR
#define GC_ARRAY_HEADER_SIZE (GC_ARRAY_COUNTER_SIZE + GC_ARRAY_LENGTH_SIZE + GC_HEADER_SIZE)
#endif

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_arrayLength* getArrayLengthp (pointer a);
static inline GC_arrayLength getArrayLength (pointer a);
static inline GC_arrayCounter* getArrayCounterp (pointer a);
static inline GC_arrayCounter getArrayCounter (pointer a);
static inline pointer indexArrayAtObjptrIndex (GC_state s, pointer a,
                                               GC_arrayCounter arrayIndex,
                                               uint32_t objptrIndex);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

uintmax_t GC_getArrayLength (pointer a);
