/* Copyright (C) 2016-2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

size_t sizeofSequenceNoMetaData (GC_state s,
                                  GC_sequenceLength numElements,
                                  uint16_t bytesNonObjptrs, uint16_t numObjptrs) {
  size_t result;

  result = numElements * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
  return alignWithExtra (s, result, GC_SEQUENCE_METADATA_SIZE);
}

size_t sizeofStackNoMetaData (__attribute__ ((unused)) GC_state s,
                              GC_stack stack) {
  size_t result;

  result = sizeof (struct GC_stack) + stack->reserved;
  return result;
}

void sizeofObjectAux (GC_state s, pointer p, size_t* metaDataBytes, size_t* objectBytes) {
  GC_header header;
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs, numObjptrs;

  header = getHeader (p);
  splitHeader (s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { 
    *metaDataBytes = GC_NORMAL_METADATA_SIZE;
    *objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  } else if (SEQUENCE_TAG == tag) {
    *metaDataBytes = GC_SEQUENCE_METADATA_SIZE;
    *objectBytes = sizeofSequenceNoMetaData (s, getSequenceLength (p),
                                          bytesNonObjptrs, numObjptrs);
  } else { /* Stack. */
    assert (STACK_TAG == tag);
    *metaDataBytes = GC_STACK_METADATA_SIZE;
    *objectBytes = sizeofStackNoMetaData (s, (GC_stack)p);
  }
}

size_t sizeofObject (GC_state s, pointer p) {
  size_t metaDataBytes, objectBytes;
  sizeofObjectAux(s, p, &metaDataBytes, &objectBytes);
  return metaDataBytes + objectBytes;
}

size_t sizeofObjectNoMetaData (GC_state s, pointer p) {
  size_t metaDataBytes, objectBytes;
  sizeofObjectAux(s, p, &metaDataBytes, &objectBytes);
  return objectBytes;
}
