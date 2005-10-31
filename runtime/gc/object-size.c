/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

size_t sizeofNumNonObjptrs (GC_objectTypeTag tag, uint16_t numNonObjptrs) {
  switch (tag) {
  case ARRAY_TAG:
    return (size_t)(numNonObjptrs);
  case NORMAL_TAG:
    return (size_t)(numNonObjptrs) * 4;
  case WEAK_TAG:
    return (size_t)(numNonObjptrs) * 4;
  default:
    die ("bad GC_objectTypeTag %u", tag);
  }
}

size_t sizeofNormalNoHeader (__attribute__ ((unused)) GC_state s,
                             uint16_t numNonObjptrs,
                             uint16_t numObjptrs) {
  size_t result;

  result = 
    sizeofNumNonObjptrs (NORMAL_TAG, numNonObjptrs)
    + (numObjptrs * OBJPTR_SIZE);
  return result;
}

size_t sizeofArrayNoHeader (GC_state s, 
                            GC_arrayLength numElements,
                            uint16_t numNonObjptrs, uint16_t numObjptrs) {
  size_t bytesPerElement;
  size_t result;
        
  bytesPerElement = 
    sizeofNumNonObjptrs (ARRAY_TAG, numNonObjptrs) 
    + (numObjptrs * OBJPTR_SIZE);
  result = numElements * bytesPerElement;
  /* Empty arrays have OBJPTR_SIZE bytes for the forwarding pointer. */
  if (0 == result) 
    result = OBJPTR_SIZE;
  return pad (s, result, GC_ARRAY_HEADER_SIZE);
}

size_t sizeofWeakNoHeader (__attribute__ ((unused)) GC_state s,
                           uint16_t numNonObjptrs,
                           uint16_t numObjptrs) {
  size_t result;

  result = 
    sizeofNumNonObjptrs (WEAK_TAG, numNonObjptrs)
    + (numObjptrs * OBJPTR_SIZE);
  return result;
}

size_t sizeofStackNoHeader (__attribute__ ((unused)) GC_state s,
                            GC_stack stack) {
  size_t result;

  result = sizeof (struct GC_stack) + stack->reserved;
  return result;
}

size_t sizeofObject (GC_state s, pointer p) {
  size_t headerBytes, objectBytes;
  GC_header header;
  GC_objectTypeTag tag;
  uint16_t numNonObjptrs, numObjptrs;
  
  header = getHeader (p);
  splitHeader (s, header, &tag, NULL, &numNonObjptrs, &numObjptrs);
  if (NORMAL_TAG == tag) { 
    headerBytes = GC_NORMAL_HEADER_SIZE;
    objectBytes = sizeofNormalNoHeader (s, numNonObjptrs, numObjptrs);
  } else if (ARRAY_TAG == tag) {
    headerBytes = GC_ARRAY_HEADER_SIZE;
    objectBytes = sizeofArrayNoHeader (s, getArrayLength (p), 
                                       numNonObjptrs, numObjptrs);
  } else if (WEAK_TAG == tag) {
    headerBytes = GC_NORMAL_HEADER_SIZE;
    objectBytes = sizeofWeakNoHeader (s, numNonObjptrs, numObjptrs);
  } else { /* Stack. */
    assert (STACK_TAG == tag);
    headerBytes = GC_STACK_HEADER_SIZE;
    objectBytes = sizeofStackNoHeader (s, (GC_stack)p);
  }
  return headerBytes + objectBytes;
}
