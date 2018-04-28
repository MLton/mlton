/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                       Object hash consing                        */
/* ---------------------------------------------------------------- */

/* Hashing based on Introduction to Algorithms by Cormen, Leiserson, and Rivest.
 * Section numbers in parens.
 * k is key to be hashed.
 * table is of size 2^p  (it must be a power of two)
 * Open addressing (12.4), meaning that we stick the entries directly in the 
 *   table and probe until we find what we want.
 * Multiplication method (12.3.2), meaning that we compute the hash by 
 *   multiplying by a magic number, chosen by Knuth, and take the high-order p
 *   bits of the low order 32 bits.
 * Double hashing (12.4), meaning that we use two hash functions, the first to
 *   decide where to start looking and a second to decide at what offset to
 *   probe.  The second hash must be relatively prime to the table size, which
 *   we ensure by making it odd and keeping the table size as a power of 2.
 */

GC_objectHashTable allocHashTable (GC_state s) {
  uint32_t elementsLengthMax;
  pointer regionStart;
  pointer regionEnd;
  GC_objectHashTable t;

  t = (GC_objectHashTable)(malloc_safe (sizeof(*t)));
  // Try to use space in the heap for the elements.
  if (not (isHeapInit (&s->secondaryHeap))) {
    if (DEBUG_SHARE)
      fprintf (stderr, "using secondaryHeap\n");
    regionStart = s->secondaryHeap.start;
    regionEnd = s->secondaryHeap.start + s->secondaryHeap.size;
  } else if (s->amInGC or not s->canMinor) {
    if (DEBUG_SHARE)
      fprintf (stderr, "using end of heap\n");
    regionStart = s->frontier;
    regionEnd = s->limitPlusSlop;
  } else {
    if (DEBUG_SHARE)
      fprintf (stderr, "using minor space\n");
    assert (s->canMinor);
    regionStart = s->heap.start + s->heap.oldGenSize;
    regionEnd = s->heap.nursery;
  }
  elementsLengthMax = (uint32_t)((size_t)(regionEnd - regionStart) / sizeof (*(t->elements)));
  if (DEBUG_SHARE)
    fprintf (stderr, "elementsLengthMax = %"PRIu32"\n", elementsLengthMax);
  t->elementsLengthMax = 64;  // some small power of two
  t->elementsLengthMaxLog2 = 6;  // and its log base 2
  if (elementsLengthMax < t->elementsLengthMax) {
    if (DEBUG_SHARE)
      fprintf (stderr, "elementsLengthMax too small -- using calloc\n");
    t->elementsIsInHeap = FALSE;
    t->elements = 
      (struct GC_objectHashElement *)
      (calloc_safe(t->elementsLengthMax, sizeof(*(t->elements))));
  } else {
    if (DEBUG_SHARE)
      fprintf (stderr, "elementsLengthMax big enough -- using heap\n");
    t->elementsIsInHeap = TRUE;
    t->elements = (struct GC_objectHashElement*)regionStart;
    // Find the largest power of two that fits.
    for ( ; 
         t->elementsLengthMax <= elementsLengthMax; 
         t->elementsLengthMax <<= 1, t->elementsLengthMaxLog2++)
      ; // nothing
    t->elementsLengthMax >>= 1;
    t->elementsLengthMaxLog2--;
    assert (t->elementsLengthMax <= elementsLengthMax);
    for (unsigned int i = 0; i < t->elementsLengthMax; ++i)
      t->elements[i].object = NULL;
  }
  t->elementsLengthCur = 0;
  t->mayInsert = TRUE;
  if (DEBUG_SHARE) {
    fprintf (stderr, "elementsIsInHeap = %s\n", 
             boolToString (t->elementsIsInHeap));
    fprintf (stderr, "elementsLengthMax = %"PRIu32"\n", t->elementsLengthMax);
    fprintf (stderr, FMTPTR" = allocHashTable ()\n", (uintptr_t)t);
  }
  return t;
}

void freeHashTable (GC_objectHashTable t) {
  unless (t->elementsIsInHeap)
    free (t->elements);
  free (t);
}

pointer insertHashTableElem (GC_state s,
                             GC_objectHashTable t,
                             GC_hash hash, pointer object,
                             pointer max, bool mightBeThere) {
  static bool init = FALSE;
  static uint64_t mult; // magic multiplier for hashing
  static uint32_t maxNumProbes = 0;

  GC_objectHashElement e;
  uint32_t numProbes;
  uint32_t probe;
  uint32_t slot; // slot in the hash table we are considering
  unsigned int *p1;
  unsigned int *p2;

  if (DEBUG_SHARE)
    fprintf (stderr, "insertHashTableElem ("FMTHASH", "FMTPTR", "FMTPTR", %s)\n",
             hash, 
             (uintptr_t)object, 
             (uintptr_t)max,
             boolToString (mightBeThere));
  if (! init) {
    init = TRUE;
    double dmult = floor (((sqrt (5.0) - 1.0) / 2.0) * (double)0x100000000llu);
    mult = (uint64_t)dmult;
  }
  slot = (uint32_t)(mult * (uint64_t)hash) >> (32 - t->elementsLengthMaxLog2);
  probe = (1 == slot % 2) ? slot : slot - 1;
  if (DEBUG_SHARE)
    fprintf (stderr, "probe = 0x%"PRIx32"\n", probe);
  assert (1 == probe % 2);
  numProbes = 0;
look:
  if (DEBUG_SHARE)
    fprintf (stderr, "slot = 0x%"PRIx32"\n", slot);
  assert (slot < t->elementsLengthMax);
  numProbes++;
  e = &t->elements[slot];
  if (NULL == e->object) {
    /* It's not in the table.  Add it. */
    unless (t->mayInsert) {
      if (DEBUG_SHARE)
        fprintf (stderr, "not inserting\n");
      return object;
    }
    e->hash = hash;
    e->object = object;
    t->elementsLengthCur++;
    if (numProbes > maxNumProbes) {
      maxNumProbes = numProbes;
      if (DEBUG_SHARE)
        fprintf (stderr, "numProbes = %"PRIu32"\n", numProbes);
    }
    return object;
  }
  unless (hash == e->hash) {
lookNext:
    slot = (slot + probe) % t->elementsLengthMax;
    goto look;
  }
  unless (mightBeThere)
    goto lookNext;
  if (DEBUG_SHARE)
    fprintf (stderr, "comparing "FMTPTR" to "FMTPTR"\n",
             (uintptr_t)object, (uintptr_t)e->object);
  /* Compare object to e->object. */
  unless (object == e->object) {
    GC_header header;
    GC_objectTypeTag tag;

    header = getHeader (object);
    unless (header == getHeader (e->object))
      goto lookNext;
    for (p1 = (unsigned int*)object, 
         p2 = (unsigned int*)e->object;
         p1 < (unsigned int*)max;
         ++p1, ++p2)
      unless (*p1 == *p2)
        goto lookNext;
    splitHeader (s, header, &tag, NULL, NULL, NULL);
    if (ARRAY_TAG == tag
        and (getArrayLength (object) != getArrayLength (e->object)))
      goto lookNext;
  }
  /* object is equal to e->object. */
  return e->object;
}

void growHashTableMaybe (GC_state s, GC_objectHashTable t) {
  GC_objectHashElement oldElement;
  struct GC_objectHashElement *oldElements;
  uint32_t oldElementsLengthMax;
  uint32_t newElementsLengthMax;

  if (not t->mayInsert or t->elementsLengthCur * 2 <= t->elementsLengthMax)
    return;
  oldElements = t->elements;
  oldElementsLengthMax = t->elementsLengthMax;
  newElementsLengthMax = oldElementsLengthMax * 2;
  if (DEBUG_SHARE)
    fprintf (stderr, 
             "trying to grow table to cardinality %"PRIu32"\n", 
             newElementsLengthMax);
  // Try to alocate the new table.
  t->elements =
    (struct GC_objectHashElement *)
    (calloc(newElementsLengthMax, sizeof(*(t->elements))));
  if (NULL == t->elements) {
    t->mayInsert = FALSE;
    t->elements = oldElements;
    if (DEBUG_SHARE)
      fprintf (stderr, "unable to grow table\n");
    return;
  }
  t->elementsLengthMax = newElementsLengthMax;
  t->elementsLengthMaxLog2++;
  for (unsigned int i = 0; i < oldElementsLengthMax; ++i) {
    oldElement = &oldElements[i];
    unless (NULL == oldElement->object)
      insertHashTableElem 
      (s, t, oldElement->hash, oldElement->object, NULL, FALSE);
  }
  if (t->elementsIsInHeap)
    t->elementsIsInHeap = FALSE;
  else
    free (oldElements);
  if (DEBUG_SHARE)
    fprintf (stderr, "done growing table\n");
}

pointer hashConsPointer (GC_state s, pointer object, bool countBytesHashConsed) {
  GC_objectHashTable t;
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  bool hasIdentity;
  GC_objectTypeTag tag;
  pointer max;
  GC_hash hash;
  GC_hash* p;
  pointer res;

  if (DEBUG_SHARE)
    fprintf (stderr, "hashConsPointer ("FMTPTR")\n", (uintptr_t)object);
  t = s->objectHashTable;
  header = getHeader (object);
  splitHeader(s, header, &tag, &hasIdentity, &bytesNonObjptrs, &numObjptrs);
  if (hasIdentity) {
    /* Don't hash cons. */
    res = object;
    goto done;
  }
  assert ((ARRAY_TAG == tag) or (NORMAL_TAG == tag));
  max = 
    object
    + (ARRAY_TAG == tag
       ? (sizeofArrayNoMetaData (s, getArrayLength (object),
                                 bytesNonObjptrs, numObjptrs))
       : (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE)));
  // Compute the hash.
  hash = (GC_hash)header;
  for (p = (GC_hash*)object; p < (GC_hash*)max; ++p)
    hash = hash * 31 + *p;
  /* Insert into table. */
  res = insertHashTableElem (s, t, hash, object, max, TRUE);
  growHashTableMaybe (s, t);
  if (countBytesHashConsed and res != object) {
    size_t amount;

    amount = (size_t)(max - object);
    if (ARRAY_TAG == tag)
      amount += GC_ARRAY_METADATA_SIZE;
    else
      amount += GC_NORMAL_METADATA_SIZE;
    s->lastMajorStatistics.bytesHashConsed += amount;
  }
done:
  if (DEBUG_SHARE)
    fprintf (stderr, FMTPTR" = hashConsPointer ("FMTPTR")\n",
             (uintptr_t)res, (uintptr_t)object);
  return res;
}

void shareObjptr (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap.start);
  if (DEBUG_SHARE)
    fprintf (stderr, "shareObjptr  opp = "FMTPTR"  *opp = "FMTOBJPTR"\n",
             (uintptr_t)opp, *opp);
  p = hashConsPointer (s, p, FALSE);
  *opp = pointerToObjptr (p, s->heap.start);
  markIntergenerationalObjptr (s, opp);
}

void printBytesHashConsedMessage (size_t bytesHashConsed, size_t bytesExamined) {
  fprintf (stderr, "[GC: hash-consed %s bytes (%.1f%% of bytes examined).]\n",
           uintmaxToCommaString(bytesHashConsed),
           100.0 * ((double)bytesHashConsed / (double)bytesExamined));
}
