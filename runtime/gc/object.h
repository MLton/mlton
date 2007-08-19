/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

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

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS)) 

static const char* objectTypeTagToString (GC_objectTypeTag tag);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */


#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Each object has a header, which immediately precedes the object data.
 * A header has the following bit layout:
 * 
 * 00        : 1
 * 01 - 19   : type index bits, index into GC_state->objectTypes.
 * 20 - 30   : counter bits, used by mark compact GC (initially 0)
 *      31   : mark bit, used by mark compact GC (initially 0)
 * 32 - 63   : 0wx00000000  (only w/ 64-bit header)
 */

#define GC_HEADER_TYPE__(z) uint ## z ## _t
#define GC_HEADER_TYPE_(z) GC_HEADER_TYPE__(z)
#define GC_HEADER_TYPE GC_HEADER_TYPE_(GC_MODEL_HEADER_SIZE)
typedef GC_HEADER_TYPE GC_header;
#define GC_HEADER_SIZE sizeof(GC_header)
#define PRIxHDR__(z) PRIx ## z
#define PRIxHDR_(z) PRIxHDR__(z)
#define PRIxHDR PRIxHDR_(GC_MODEL_HEADER_SIZE)
#define FMTHDR "%08"PRIxHDR

COMPILE_TIME_ASSERT(sizeof_objptr__eq__sizeof_header,
                    sizeof(objptr) == sizeof(GC_header));

#define GC_VALID_HEADER_MASK ((GC_header)0x1)
#define TYPE_INDEX_BITS    19
#define TYPE_INDEX_MASK    ((GC_header)0x000FFFFE)
#define TYPE_INDEX_SHIFT   1
#define COUNTER_BITS       10
#define COUNTER_MASK       ((GC_header)0x7FF00000)
#define COUNTER_SHIFT      20
#define MARK_BITS          1
#define MARK_MASK          ((GC_header)0x80000000)
#define MARK_SHIFT         31

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_header* getHeaderp (pointer p);
static inline GC_header getHeader (pointer p);
static inline GC_header buildHeaderFromTypeIndex (uint32_t t);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */


#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Normal objects have the following layout:
 *
 * header :: 
 * (non heap-pointers)* :: 
 * (heap pointers)*
 *
 * Note that the non heap-pointers denote a sequence of primitive data
 * values.  These data values need not map directly to values of the
 * native word size.  MLton's aggressive representation strategies may
 * pack multiple primitive values into the same native word.
 * Likewise, a primitive value may span multiple native words (e.g.,
 * Word64.word on an x86).
*/
#define GC_NORMAL_HEADER_SIZE GC_HEADER_SIZE

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */


/* Array objects are described in "array.h" */

/* Stack objects are described in "stack.h" */

/* Weak objects are described in "weak.h" */


#if (defined (MLTON_GC_INTERNAL_TYPES))

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
 * object, the bytesNonObjptrs field indicates the number of bytes of
 * non heap-pointer data, while the numObjptrs field indicates the
 * number of heap pointers.  In an array object, the bytesNonObjptrs
 * field indicates the number of bytes of non heap-pointer data in a
 * single array element, while the numObjptrs field indicates the
 * number of heap pointers in a single array element.  In a stack
 * object, the bytesNonObjptrs and numObjptrs fields are irrelevant.
 * In a weak object, the bytesNonObjptrs and numObjptrs fields are
 * interpreted as in a normal object.
*/
typedef struct GC_objectType {
  /* Keep tag first, at zero offset, since it is referenced most often. */
  GC_objectTypeTag tag;
  bool hasIdentity;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
} *GC_objectType;
enum {
  /* The type indices here must agree with those in backend/rep-type.fun. */
  STACK_TYPE_INDEX =         0,
  THREAD_TYPE_INDEX =        1,
  WEAK_GONE_TYPE_INDEX =     2,
  WORD8_VECTOR_TYPE_INDEX =  3,
  WORD32_VECTOR_TYPE_INDEX = 4,
  WORD16_VECTOR_TYPE_INDEX = 5,
  WORD64_VECTOR_TYPE_INDEX = 6,
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define GC_STACK_HEADER buildHeaderFromTypeIndex (STACK_TYPE_INDEX)
#define GC_THREAD_HEADER buildHeaderFromTypeIndex (THREAD_TYPE_INDEX)
#define GC_WEAK_GONE_HEADER buildHeaderFromTypeIndex (WEAK_GONE_TYPE_INDEX)
#define GC_WORD8_VECTOR_HEADER buildHeaderFromTypeIndex (WORD8_VECTOR_TYPE_INDEX)
#define GC_WORD16_VECTOR_HEADER buildHeaderFromTypeIndex (WORD16_VECTOR_TYPE_INDEX)
#define GC_WORD32_VECTOR_HEADER buildHeaderFromTypeIndex (WORD32_VECTOR_TYPE_INDEX)
#define GC_WORD64_VECTOR_HEADER buildHeaderFromTypeIndex (WORD64_VECTOR_TYPE_INDEX)

static inline void splitHeader (GC_state s, GC_header header,
                                GC_objectTypeTag *tagRet, bool *hasIdentityRet,
                                uint16_t *bytesNonObjptrsRet, uint16_t *numObjptrsRet);
static inline pointer advanceToObjectData (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
