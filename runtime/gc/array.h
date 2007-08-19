/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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
 * counter :: 
 * length :: 
 * header :: 
 * ( (non heap-pointers)* :: (heap pointers)* )*
 *
 * The counter word is used by mark compact GC.  The length word is
 * the number of elements in the array.  Array elements have the same
 * individual layout as normal objects, omitting the header word.
 */

#define GC_ARRLEN_TYPE__(z) uint ## z ## _t
#define GC_ARRLEN_TYPE_(z) GC_ARRLEN_TYPE__(z)
#define GC_ARRLEN_TYPE GC_ARRLEN_TYPE_(GC_MODEL_ARRLEN_SIZE)
typedef GC_ARRLEN_TYPE GC_arrayLength;
#define GC_ARRAY_LENGTH_SIZE sizeof(GC_arrayLength)
#define PRIxARRLEN__(z) PRIx ## z
#define PRIxARRLEN_(z) PRIxARRLEN__(z)
#define PRIxARRLEN PRIxARRLEN_(GC_MODEL_ARRLEN_SIZE)
#define FMTARRLEN "%"PRIxARRLEN
typedef GC_arrayLength GC_arrayCounter;
#define GC_ARRAY_COUNTER_SIZE sizeof(GC_arrayCounter)
#define PRIxARRCTR PRIxARRLEN
#define FMTARRCTR "%"PRIxARRCTR
#define GC_ARRAY_HEADER_SIZE (GC_ARRAY_COUNTER_SIZE + GC_ARRAY_LENGTH_SIZE + GC_HEADER_SIZE)

COMPILE_TIME_ASSERT(sizeof_header__le__sizeof_arrlen,
                    sizeof(GC_header) <= sizeof(GC_arrayLength));
COMPILE_TIME_ASSERT(sizeof_arrlen__eq__sizeof_arrctr,
                    sizeof(GC_arrayLength) == sizeof(GC_arrayCounter));


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
