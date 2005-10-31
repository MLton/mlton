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

const char* objectTypeTagToString (GC_objectTypeTag tag);

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
#define GC_HEADER_SIZE sizeof(GC_header)
#define PRIxHDR PRIx32
#define FMTHDR "0x%08"PRIxHDR

#define GC_VALID_HEADER_MASK ((GC_header)0x1)
#define TYPE_INDEX_BITS    19
#define TYPE_INDEX_MASK    0x000FFFFE
#define TYPE_INDEX_SHIFT   1
#define COUNTER_BITS       10
#define COUNTER_MASK       0x7FF00000
#define COUNTER_SHIFT      20
#define MARK_BITS          1
#define MARK_MASK          0x80000000
#define MARK_SHIFT         31

GC_header* getHeaderp (pointer p);
GC_header getHeader (pointer p);
GC_header buildHeaderFromTypeIndex (uint32_t t);

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
#define GC_NORMAL_HEADER_SIZE GC_HEADER_SIZE

/* Array objects are described in "array.h" */

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
 * object, the numNonObjptrs field indicates the number of 32-bit
 * words of non heap-pointer data, while the numObjptrs field
 * indicates the number of heap pointers.  In an array object, the
 * numNonObjptrs field indicates the number of bytes of non
 * heap-pointer data, while the numObjptrs field indicates the number
 * of heap pointers.  In a stack object, the numNonObjptrs and
 * numObjptrs fields are irrelevant.  In a weak object, the
 * numNonObjptrs and numObjptrs fields are interpreted as in a normal
 * object (and, hence, must be (2,1) or (3,0)).
*/
typedef struct {
  /* Keep tag first, at zero offset, since it is referenced most often. */
  GC_objectTypeTag tag;
  bool hasIdentity;
  uint16_t numNonObjptrs;
  uint16_t numObjptrs;
} *GC_objectType;
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

#define GC_STACK_HEADER buildHeaderFromTypeIndex (STACK_TYPE_INDEX)
#define GC_THREAD_HEADER buildHeaderFromTypeIndex (THREAD_TYPE_INDEX)
#define GC_WEAK_GONE_HEADER buildHeaderFromTypeIndex (WEAK_GONE_TYPE_INDEX)
#define GC_WORD8_VECTOR_HEADER buildHeaderFromTypeIndex (WORD8_VECTOR_TYPE_INDEX)
#define GC_WORD16_VECTOR_HEADER buildHeaderFromTypeIndex (WORD16_VECTOR_TYPE_INDEX)
#define GC_WORD32_VECTOR_HEADER buildHeaderFromTypeIndex (WORD32_VECTOR_TYPE_INDEX)

void splitHeader (GC_state s, GC_header header,
                  GC_objectTypeTag *tagRet, bool *hasIdentityRet,
                  uint16_t *numNonObjptrsRet, uint16_t *numObjptrsRet);
pointer advanceToObjectData (GC_state s, pointer p);
