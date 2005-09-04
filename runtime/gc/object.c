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
    int objectTypeIndex;                                                        \
    GC_objectType *t;                                                           \
                                                                                \
    assert (1 == (header & 1));                                                 \
    objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;           \
    assert (0 <= objectTypeIndex                                                \
            and objectTypeIndex < s->objectTypesSize);                          \
    t = &s->objectTypes [objectTypeIndex];                                      \
    tag = t->tag;                                                               \
    hasIdentity = t->hasIdentity;                                               \
    numNonPointers = t->numNonPointers;                                         \
    numPointers = t->numPointers;                                               \
    if (DEBUG_DETAILED)                                                         \
      fprintf (stderr,                                                          \
               "SPLIT_HEADER ("FMTHDR")"                                        \
               "  tag = %s"                                                     \
               "  hasIdentity = %u"                                             \
               "  numNonPointers = %"PRIu16                                     \
               "  numPointers = %"PRIu16"\n",                                   \
               header,                                                          \
               tagToString(tag), hasIdenity, numNonPointers, numPointers);      \
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
