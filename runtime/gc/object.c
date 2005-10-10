/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static char* objectTypeTagToString (GC_objectTypeTag tag) {
  switch (tag) {
  case ARRAY_TAG:
    return "ARRAY";
  case NORMAL_TAG:
    return "NORMAL";
  case STACK_TAG:
    return "STACK";
  case WEAK_TAG:
    return "WEAK";
  default:
    die ("bad tag %u", tag);
  }
}

/*
 * Build the header for an object, given the index to its type info.
 */
static inline GC_header objectHeader (uint32_t t) {
        assert (t < TWOPOWER (TYPE_INDEX_BITS));
        return 1 | (t << 1);
}

#define GC_STACK_HEADER objectHeader (STACK_TYPE_INDEX)
#define GC_STRING_HEADER objectHeader (STRING_TYPE_INDEX)
#define GC_THREAD_HEADER objectHeader (THREAD_TYPE_INDEX)
#define GC_WEAK_GONE_HEADER objectHeader (WEAK_GONE_TYPE_INDEX)
#define GC_WORD8_VECTOR_HEADER objectHeader (WORD8_TYPE_INDEX)

static inline void splitHeader(GC_state s, GC_header header,
                               GC_objectTypeTag *tagRet, bool *hasIdentityRet,
                               uint16_t *numNonObjptrsRet, uint16_t *numObjptrsRet) {
  unsigned int objectTypeIndex; 
  GC_objectType *objectType; 
  GC_objectTypeTag tag;
  bool hasIdentity;
  uint16_t numNonObjptrs, numObjptrs;

  assert (1 == (header & GC_VALID_HEADER_MASK)); 
  objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT; 
  assert (objectTypeIndex < s->objectTypesLength); 
  objectType = &s->objectTypes [objectTypeIndex]; 
  tag = objectType->tag; 
  hasIdentity = objectType->hasIdentity; 
  numNonObjptrs = objectType->numNonObjptrs; 
  numObjptrs = objectType->numObjptrs; 

  if (DEBUG_DETAILED) 
    fprintf (stderr, 
             "splitHeader ("FMTHDR")" 
             "  tag = %s" 
             "  hasIdentity = %u" 
             "  numNonObjptrs = %"PRIu16 
             "  numObjptrs = %"PRIu16"\n", 
             header, 
             objectTypeTagToString(tag), hasIdentity, numNonObjptrs, numObjptrs); 

  if (tagRet != NULL)
    *tagRet = tag;
  if (hasIdentityRet != NULL)
    *hasIdentityRet = hasIdentity;
  if (numNonObjptrsRet != NULL)
    *numNonObjptrsRet = numNonObjptrs;
  if (numObjptrsRet != NULL)
    *numObjptrsRet = numObjptrs;
}

/* objectData (s, p)
 *
 * If p points at the beginning of an object, then objectData returns
 * a pointer to the start of the object data.
 */
static inline pointer objectData (GC_state s, pointer p) {
  GC_header header;
  pointer res;

  assert (isAlignedFrontier (s, p));
  header = *(GC_header*)p;
  if (0 == header)
    /* Looking at the counter word in an array. */
    res = p + GC_ARRAY_HEADER_SIZE;
  else
    /* Looking at a header word. */
    res = p + GC_NORMAL_HEADER_SIZE;
  assert (isAligned ((uintptr_t)res, s->alignment));
  return res;
}
