/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if ASSERT
static inline pointer arrayIndexAtPointer (GC_state s, 
                                           pointer a, 
                                           uint32_t arrayIndex, 
                                           uint32_t pointerIndex) {
  bool hasIdentity;
  GC_header header;
  uint16_t numNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  
  header = getHeader (a);
  SPLIT_HEADER();
  assert (tag == ARRAY_TAG);

  size_t bytesPerElement = 
    numNonObjptrsToBytes(numNonObjptrs, ARRAY_TAG) 
    + (numObjptrs * OBJPTR_SIZE);

  return a
    + arrayIndex * bytesPerElement
    + numNonObjptrsToBytes(numNonObjptrs, tag) 
    + pointerIndex * OBJPTR_SIZE;
}
#endif

/* The number of bytes in an array, not including the header. */
static inline size_t arrayNumBytes (GC_state s,
                                    pointer p, 
                                    uint16_t numObjptrs,
                                    uint16_t numNonObjptrs) {
  size_t bytesPerElement;
  GC_arrayLength numElements;
  size_t result;
        
  numElements = arrayNumElements (p);
  bytesPerElement = 
    numNonObjptrsToBytes(numNonObjptrs, ARRAY_TAG) 
    + (numObjptrs * OBJPTR_SIZE);
  result = numElements * bytesPerElement;
  /* Empty arrays have OBJPTR_SIZE bytes for the forwarding pointer. */
  if (0 == result) 
    result = OBJPTR_SIZE;
  return pad (s, result, GC_ARRAY_HEADER_SIZE);
}
