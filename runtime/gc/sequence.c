/* Copyright (C) 2012,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* getSequenceLengthp (p)
 *
 * Returns a pointer to the length for the sequence pointed to by p.
 */
GC_sequenceLength* getSequenceLengthp (pointer a) {
  return (GC_sequenceLength*)(a
                              - GC_HEADER_SIZE
                              - GC_SEQUENCE_LENGTH_SIZE);
}

/* getSequenceLength (p)
 *
 * Returns the length for the sequence pointed to by p.
 */
GC_sequenceLength getSequenceLength (pointer a) {
  return *(getSequenceLengthp (a));
}

uintmax_t GC_getSequenceLength (pointer a) {
  return ((uintmax_t)(getSequenceLength (a)));
}

/* getSequenceCounterp (p)
 *
 * Returns a pointer to the counter for the sequence pointed to by p.
 */
GC_sequenceCounter* getSequenceCounterp (pointer a) {
  return (GC_sequenceCounter*)(a
                               - GC_HEADER_SIZE
                               - GC_SEQUENCE_LENGTH_SIZE
                               - GC_SEQUENCE_COUNTER_SIZE);
}

/* getSequenceCounter (p)
 *
 * Returns the counter for the sequence pointed to by p.
 */
GC_sequenceCounter getSequenceCounter (pointer a) {
  return *(getSequenceCounterp (a));
}


#if ASSERT
pointer indexSequenceAtObjptrIndex (GC_state s, pointer a,
                                    GC_sequenceCounter sequenceIndex,
                                    uint32_t objptrIndex) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  header = getHeader (a);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  assert (tag == SEQUENCE_TAG);

  return a
    + (sequenceIndex * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE)))
    + bytesNonObjptrs
    + (objptrIndex * OBJPTR_SIZE);
}
#endif


/* GC_sequenceCopy (ad, as, as, ss, l)
 *
 * Copy l elements of as starting at ss to ad starting at as.
 */
void GC_sequenceCopy (GC_state s, pointer ad, size_t ds, pointer as, size_t ss, size_t l) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  size_t eltSize;

  header = getHeader (ad);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  assert (tag == SEQUENCE_TAG);

  if (numObjptrs > 0 and l > 0) {
    markCard (s, ad);
  }

  eltSize = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  GC_memmove (as + eltSize * ss, ad + eltSize * ds, eltSize * l);
}
