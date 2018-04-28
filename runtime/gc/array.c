/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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

uintmax_t GC_getArrayLength (pointer a) {
  return ((uintmax_t)(getArrayLength (a)));
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


#if ASSERT
pointer indexArrayAtObjptrIndex (GC_state s, pointer a,
                                 GC_arrayCounter arrayIndex,
                                 uint32_t objptrIndex) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  header = getHeader (a);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  assert (tag == ARRAY_TAG);

  return a
    + (arrayIndex * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE)))
    + bytesNonObjptrs
    + (objptrIndex * OBJPTR_SIZE);
}
#endif


/* GC_arrayCopy (ad, as, as, ss, l)
 *
 * Copy l elements of as starting at ss to ad starting at as.
 */
void GC_arrayCopy (GC_state s, pointer ad, size_t ds, pointer as, size_t ss, size_t l) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  size_t eltSize;

  header = getHeader (ad);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  assert (tag == ARRAY_TAG);

  eltSize = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  GC_memmove (as + eltSize * ss, ad + eltSize * ds, eltSize * l);
}
