/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/*
 * Build the header for an object, given the index to its type info.
 */
static inline GC_header GC_objectHeader (uint32_t t) {
        assert (t < TWOPOWER (TYPE_INDEX_BITS));
        return 1 | (t << 1);
}

#define STACK_HEADER GC_objectHeader (STACK_TYPE_INDEX)
#define STRING_HEADER GC_objectHeader (STRING_TYPE_INDEX)
#define THREAD_HEADER GC_objectHeader (THREAD_TYPE_INDEX)
#define WEAK_GONE_HEADER GC_objectHeader (WEAK_GONE_TYPE_INDEX)
#define WORD8_VECTOR_HEADER GC_objectHeader (WORD8_TYPE_INDEX)

#define SPLIT_HEADER()                                                          \
  do {                                                                          \
    unsigned int objectTypeIndex;                                               \
    GC_objectType *t;                                                           \
                                                                                \
    assert (1 == (header & GC_VALID_HEADER_MASK));                              \
    objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;           \
    assert (objectTypeIndex < s->objectTypesSize);                              \
    t = &s->objectTypes [objectTypeIndex];                                      \
    tag = t->tag;                                                               \
    hasIdentity = t->hasIdentity;                                               \
    numNonObjptrs = t->numNonObjptrs;                                           \
    numObjptrs = t->numObjptrs;                                                 \
    if (DEBUG_DETAILED)                                                         \
      fprintf (stderr,                                                          \
               "SPLIT_HEADER ("FMTHDR")"                                        \
               "  tag = %s"                                                     \
               "  hasIdentity = %u"                                             \
               "  numNonObjptrs = %"PRIu16                                      \
               "  numObjptrs = %"PRIu16"\n",                                    \
               header,                                                          \
               tagToString(tag), hasIdentity, numNonObjptrs, numObjptrs);       \
  } while (0)

static char* tagToString (GC_objectTypeTag tag) {
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

/* If p points at the beginning of an object, then toData p returns a
 * pointer to the start of the object data.
 */
static inline pointer toData (GC_state s, pointer p) {
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

static inline size_t numNonObjptrsToBytes (uint16_t numNonObjptrs, 
                                           GC_objectTypeTag tag) {
  switch (tag) {
  case ARRAY_TAG:
    return (size_t)(numNonObjptrs);
  case NORMAL_TAG:
    return (size_t)(numNonObjptrs) * 4;
  case WEAK_TAG:
    return (size_t)(numNonObjptrs) * 4;
  default:
    die ("bad tag %u", tag);
  }
}
