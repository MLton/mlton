/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* getArrayLengthp (p)
 *
 * Returns a pointer to the length for the array pointed to by p.
 */
GC_arrayLength* getArrayLengthp (pointer a) {
  return (GC_arrayLength*)(a 
                           - GC_HEADER_SIZE 
                           - GC_ARRAY_LENGTH_SIZE);
}

/* getArrayLength (p)
 *
 * Returns the length for the array pointed to by p.
 */
GC_arrayLength getArrayLength (pointer a) {
  return *(getArrayLengthp (a));
}

/* getArrayCounterp (p)
 *
 * Returns a pointer to the counter for the array pointed to by p.
 */
GC_arrayCounter* getArrayCounterp (pointer a) {
  return (GC_arrayCounter*)(a 
                            - GC_HEADER_SIZE 
                            - GC_ARRAY_LENGTH_SIZE 
                            - GC_ARRAY_COUNTER_SIZE);
}

/* getArrayCounter (p)
 *
 * Returns the counter for the array pointed to by p.
 */
GC_arrayCounter getArrayCounter (pointer a) {
  return *(getArrayCounterp (a));
}

pointer indexArrayAtPointerIndex (GC_state s, pointer a,
                                  GC_arrayCounter arrayIndex,
                                  uint32_t pointerIndex) {
  GC_header header;
  uint16_t numNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  
  header = getHeader (a);
  splitHeader(s, header, &tag, NULL, &numNonObjptrs, &numObjptrs);
  assert (tag == ARRAY_TAG);

  size_t nonObjptrBytesPerElement =
    sizeofNumNonObjptrs (ARRAY_TAG, numNonObjptrs);
  size_t bytesPerElement =
    nonObjptrBytesPerElement
    + (numObjptrs * OBJPTR_SIZE);

  return a
    + arrayIndex * bytesPerElement
    + nonObjptrBytesPerElement
    + pointerIndex * OBJPTR_SIZE;
}


GC_arrayLength GC_getArrayLength (pointer a) {
  return getArrayLength (a);
}
