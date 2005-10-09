/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline size_t objectSize (GC_state s, pointer p) {
  size_t headerBytes, objectBytes;
  GC_header header;
  GC_objectTypeTag tag;
  uint16_t numNonObjptrs, numObjptrs;
  
  header = getHeader (p);
  splitHeader (s, header, &tag, NULL, &numNonObjptrs, &numObjptrs);
  if (NORMAL_TAG == tag) { /* Fixed size object. */
    headerBytes = GC_NORMAL_HEADER_SIZE;
    objectBytes = 
      numNonObjptrsToBytes (numNonObjptrs, NORMAL_TAG)
      + (numObjptrs * OBJPTR_SIZE);
  } else if (ARRAY_TAG == tag) {
    headerBytes = GC_ARRAY_HEADER_SIZE;
    objectBytes = arrayNumBytes (s, p, numNonObjptrs, numObjptrs);
  } else if (WEAK_TAG == tag) {
    headerBytes = GC_NORMAL_HEADER_SIZE;
    objectBytes = 
      numNonObjptrsToBytes (numNonObjptrs, NORMAL_TAG)
      + (numObjptrs * OBJPTR_SIZE);
  } else { /* Stack. */
    assert (STACK_TAG == tag);
    headerBytes = GC_STACK_HEADER_SIZE;
    objectBytes = sizeof (struct GC_stack) + ((GC_stack)p)->reserved;
  }
  return headerBytes + objectBytes;
}
