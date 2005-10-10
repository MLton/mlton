/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

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

static inline size_t arraySizeNoHeader (GC_state s,
                                        pointer p, 
                                        uint16_t numNonObjptrs,
                                        uint16_t numObjptrs) {
  size_t bytesPerElement;
  GC_arrayLength numElements;
  size_t result;
        
  numElements = getArrayLength (p);
  bytesPerElement = 
    numNonObjptrsToBytes(numNonObjptrs, ARRAY_TAG) 
    + (numObjptrs * OBJPTR_SIZE);
  result = numElements * bytesPerElement;
  /* Empty arrays have OBJPTR_SIZE bytes for the forwarding pointer. */
  if (0 == result) 
    result = OBJPTR_SIZE;
  return pad (s, result, GC_ARRAY_HEADER_SIZE);
}

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
    objectBytes = arraySizeNoHeader (s, p, numNonObjptrs, numObjptrs);
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


static inline size_t stackNumBytes (GC_state s, size_t size) {
  size_t res;
  
  res = align (GC_STACK_HEADER_SIZE + sizeof (struct GC_stack) + size,
               s->alignment);
  if (DEBUG_STACKS)
    fprintf (stderr, "%zu = stackNumBytes (%zu)\n", res, size);
  return res;
}
