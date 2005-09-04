/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* 
 * There are four kinds of ML objects: 
 *   array, normal (fixed size), stack, and weak.
 */
typedef enum { 
        ARRAY_TAG,
        NORMAL_TAG,
        STACK_TAG,
        WEAK_TAG,
} GC_objectTypeTag;

/*
 * Each object has a header, which immediately precedes the object data.
 * A header has the following bit layout:
 * 
 * 00        : 1
 * 01 - 19   : type index bits, index into GC_state->objectTypes.
 * 20 - 30   : counter bits, used by mark compact GC (initially 0)
 *      31   : mark bit, used by mark compact GC (initially 0)
 */
typedef uint32_t GC_header;
#define PRIxHDR PRIx32
#define FMTHDR "0x%08"PRIxHDR
enum {
  GC_HEADER_SIZE =   sizeof(GC_header),
  TYPE_INDEX_BITS =  19,
  TYPE_INDEX_MASK =  0x000FFFFE,
  TYPE_INDEX_SHIFT = 1,
  COUNTER_BITS =     10,
  COUNTER_MASK =     0x7FF00000,
  COUNTER_SHIFT =    20,
  MARK_BITS =        1,
  MARK_MASK =        0x80000000,
  MARK_SHIFT =       31
};

/*
 * Normal objects have the following layout:
 *
 * header word32 :: 
 * (non heap-pointers)* :: 
 * (heap pointers)*
 *
 * Note that the non heap-pointers denote a sequence of primitive data
 * values.  These data values need not map directly to values of the
 * native word size.  MLton's aggressive representation strategies may
 * pack multiple primitive values into the same native word.
 * Likewise, a primitive value may span multiple native words (e.g.,
 * Word64.word).
*/
enum {
  GC_NORMAL_HEADER_SIZE = GC_HEADER_SIZE,
};

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
enum {
  GC_ARRAY_LENGTH_SIZE =  4,
  GC_ARRAY_COUNTER_SIZE = GC_ARRAY_LENGTH_SIZE,
  GC_ARRAY_HEADER_SIZE =  GC_ARRAY_COUNTER_SIZE + GC_ARRAY_LENGTH_SIZE + GC_HEADER_SIZE,
};

/* Stack objects are described in "stack.h" */

/* Weak objects are described in "weak.h" */


/* 
 * The type index of a header is an index into an array of object
 * types, where each element describes the layout of an object.  The
 * object types array is declared as:
 *
 *  GC_objectType *objectTypes;
 *
 * The objectTypes pointer is initialized to point to a static array
 * of object types that is emitted for each compiled program.  The
 * hasIdentity field indicates whether or not the object has mutable
 * fields, in which case it may not be hash-cons-ed.  In a normal
 * object, the numNonPointers field indicates the number of 32-bit
 * words of non heap-pointer data, while the numPointers field
 * indicates the number of heap pointers.  In an array object, the
 * numNonPointers field indicates the number of bytes of non
 * heap-pointer data, while the numPointers field indicates the number
 * of heap pointers.  In a stack object, the numNonPointers and
 * numPointers fields are irrelevant.  In a weak object, the
 * numNonPointers and numPointers fields are interpreted as in a
 * normal object (and, hence, must be (0,1) or (0,0)).
*/
typedef struct {
        /* Keep tag first, at zero offset, since it is referenced most often. */
        GC_objectTypeTag tag;
        bool hasIdentity;
        uint16_t numNonPointers;
        uint16_t numPointers;
} GC_objectType;
enum {
  /* The type indices here must agree with those in backend/rep-type.fun. */
  STACK_TYPE_INDEX =         0,
  STRING_TYPE_INDEX =        1,
  THREAD_TYPE_INDEX =        2,
  WEAK_GONE_TYPE_INDEX =     3,
  WORD8_VECTOR_TYPE_INDEX =  STRING_TYPE_INDEX,
  WORD32_VECTOR_TYPE_INDEX = 4,
  WORD16_VECTOR_TYPE_INDEX = 5,
};
