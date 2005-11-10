/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

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
typedef uint32_t GC_arrayLength;
#define GC_ARRAY_LENGTH_SIZE sizeof(GC_arrayLength)
#define PRIxARRLEN PRIu32
#define FMTARRLEN "%"PRIxARRLEN
typedef GC_arrayLength GC_arrayCounter;
#define GC_ARRAY_COUNTER_SIZE sizeof(GC_arrayCounter)
#define PRIxARRCTR PRIu32
#define FMTARRCTR "%"PRIxARRCTR
#define GC_ARRAY_HEADER_SIZE (GC_ARRAY_COUNTER_SIZE + GC_ARRAY_LENGTH_SIZE + GC_HEADER_SIZE)

GC_arrayLength* getArrayLengthp (pointer a);
GC_arrayLength getArrayLength (pointer a);
GC_arrayCounter* getArrayCounterp (pointer a);
GC_arrayCounter getArrayCounter (pointer a);
pointer indexArrayAtPointerIndex (GC_state s, pointer a,
                                  GC_arrayCounter arrayIndex,
                                  uint32_t pointerIndex);
