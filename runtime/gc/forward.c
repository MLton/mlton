/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if ASSERT
bool isObjptrInToSpace (GC_state s, objptr op) {
  pointer p;

  if (not (isObjptr (op)))
    return TRUE;
  p = objptrToPointer (op, s->forwardState.toStart);
  return isPointerInToSpace (s, p);
}
#endif

bool isPointerInToSpace (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->forwardState.toStart <= p and p < s->forwardState.toLimit));
}

/* copyObjptr (s, opp)
 * Copies the object pointed to by *opp
 */
void copyObjptr (GC_state s, objptr *opp) {
  objptr op;
  pointer p;
  GC_header header;

  op = *opp;
  p = objptrToPointer (op, s->heap.start);
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "copyObjptr opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR"\n",
             (uintptr_t)opp, op, (uintptr_t)p);
  assert (isObjptrInFromSpace (s, *opp));
  header = getHeader (p);

  CopyObjectMap* e = NULL;
  HASH_FIND_PTR (s->copyObjectMap, &p, e);
  if (e) { //We have already copied the object to toSpace
    *opp = (objptr)e->newP;
    if (DEBUG_DETAILED)
      fprintf (stderr, "copyObjptr: Already copied newP="FMTPTR"\n", (uintptr_t)*opp);
    return;
  }

  size_t size, skip;

  size_t headerBytes, objectBytes;
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs, numObjptrs;

  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

  /* Compute the space taken by the header and object body. */
  if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
    headerBytes = GC_NORMAL_HEADER_SIZE;
    objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    skip = 0;
  } else if (ARRAY_TAG == tag) {
    headerBytes = GC_ARRAY_HEADER_SIZE;
    objectBytes = sizeofArrayNoHeader (s, getArrayLength (p),
                                        bytesNonObjptrs, numObjptrs);
    skip = 0;
  } else { /* Stack. */
    bool current;
    size_t reservedNew;
    GC_stack stack;

    assert (STACK_TAG == tag);
    headerBytes = GC_STACK_HEADER_SIZE;
    stack = (GC_stack)p;
    current = getStackCurrent(s) == stack;

    reservedNew = sizeofStackShrinkReserved (s, stack, current);
    if (reservedNew < stack->reserved) {
      if (DEBUG_STACKS or s->controls.messages)
        fprintf (stderr,
                  "[GC: Shrinking stack of size %s bytes to size %s bytes, using %s bytes.]\n",
                  uintmaxToCommaString(stack->reserved),
                  uintmaxToCommaString(reservedNew),
                  uintmaxToCommaString(stack->used));
      stack->reserved = reservedNew;
    }
    objectBytes = sizeof (struct GC_stack) + stack->used;
    skip = stack->reserved - stack->used;
  }
  size = headerBytes + objectBytes;
  assert (s->forwardState.back + size + skip <= s->forwardState.toLimit);
  /* Copy the object. */
  GC_memcpy (p - headerBytes, s->forwardState.back, size);
  /* If the object has a valid weak pointer, link it into the weaks
    * for update after the copying GC is done.
    */
  if ((WEAK_TAG == tag) and (numObjptrs == 1)) {
    GC_weak w;

    w = (GC_weak)(s->forwardState.back + GC_NORMAL_HEADER_SIZE + offsetofWeak (s));
    if (DEBUG_WEAK)
      fprintf (stderr, "forwarding weak "FMTPTR" ",
                (uintptr_t)w);
    if (isObjptr (w->objptr)
        and (not s->forwardState.amInMinorGC
              or isObjptrInNursery (s, w->objptr))) {
      if (DEBUG_WEAK)
        fprintf (stderr, "linking\n");
      w->link = s->weaks;
      s->weaks = w;
    } else {
      if (DEBUG_WEAK)
        fprintf (stderr, "not linking\n");
    }
  }

  e = (CopyObjectMap*) malloc (sizeof (CopyObjectMap));
  e->oldP = (pointer)*opp;
  e->newP = (pointer) s->forwardState.back + headerBytes;
  if (DEBUG_DETAILED)
    fprintf (stderr, "copyObjptr: Adding oldP="FMTPTR" newP="FMTPTR"\n",
             (uintptr_t)e->oldP, (uintptr_t)e->newP);
  HASH_ADD_PTR (s->copyObjectMap, oldP, e);

  /* Only update the pointer if it is in toSpace to avoid mucking with the
   * original object. */
  if (isPointerInToSpace (s, (pointer)opp)) {
    *opp = pointerToObjptr (s->forwardState.back + headerBytes,
                            s->forwardState.toStart);
    if (DEBUG_DETAILED)
      fprintf (stderr, "copyObjptr --> *opp = "FMTPTR"\n", (uintptr_t)*opp);
  }

  /* Update the back of the queue. */
  s->forwardState.back += skip + size;
  assert (isAligned ((size_t)s->forwardState.back + GC_NORMAL_HEADER_SIZE,
                      s->alignment));
}


/* forward (s, opp)
 * Forwards the object pointed to by *opp and updates *opp to point to
 * the new object.
 */
void forwardObjptr (GC_state s, objptr *opp) {
  objptr op;
  pointer p;
  GC_header header;

  op = *opp;
  p = objptrToPointer (op, s->heap.start);
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "forwardObjptr  opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR"\n",
             (uintptr_t)opp, op, (uintptr_t)p);
  assert (isObjptrInFromSpace (s, *opp));
  header = getHeader (p);
  if (DEBUG_DETAILED and header == GC_FORWARDED)
    fprintf (stderr, "  already FORWARDED\n");
  if (header != GC_FORWARDED) { /* forward the object */
    size_t size, skip;

    size_t headerBytes, objectBytes;
    GC_objectTypeTag tag;
    uint16_t bytesNonObjptrs, numObjptrs;

    splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

    /* Compute the space taken by the header and object body. */
    if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
      headerBytes = GC_NORMAL_HEADER_SIZE;
      objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
      skip = 0;
    } else if (ARRAY_TAG == tag) {
      headerBytes = GC_ARRAY_HEADER_SIZE;
      objectBytes = sizeofArrayNoHeader (s, getArrayLength (p),
                                         bytesNonObjptrs, numObjptrs);
      skip = 0;
    } else { /* Stack. */
      bool current;
      size_t reservedNew;
      GC_stack stack;

      assert (STACK_TAG == tag);
      headerBytes = GC_STACK_HEADER_SIZE;
      stack = (GC_stack)p;
      current = getStackCurrent(s) == stack;

      reservedNew = sizeofStackShrinkReserved (s, stack, current);
      if (reservedNew < stack->reserved) {
        if (DEBUG_STACKS or s->controls.messages)
          fprintf (stderr,
                   "[GC: Shrinking stack of size %s bytes to size %s bytes, using %s bytes.]\n",
                   uintmaxToCommaString(stack->reserved),
                   uintmaxToCommaString(reservedNew),
                   uintmaxToCommaString(stack->used));
        stack->reserved = reservedNew;
      }
      objectBytes = sizeof (struct GC_stack) + stack->used;
      skip = stack->reserved - stack->used;
    }
    size = headerBytes + objectBytes;
    assert (s->forwardState.back + size + skip <= s->forwardState.toLimit);
    /* Copy the object. */
    GC_memcpy (p - headerBytes, s->forwardState.back, size);
    /* If the object has a valid weak pointer, link it into the weaks
     * for update after the copying GC is done.
     */
    if ((WEAK_TAG == tag) and (numObjptrs == 1)) {
      GC_weak w;

      w = (GC_weak)(s->forwardState.back + GC_NORMAL_HEADER_SIZE + offsetofWeak (s));
      if (DEBUG_WEAK)
        fprintf (stderr, "forwarding weak "FMTPTR" ",
                 (uintptr_t)w);
      if (isObjptr (w->objptr)
          and (not s->forwardState.amInMinorGC
               or isObjptrInNursery (s, w->objptr))) {
        if (DEBUG_WEAK)
          fprintf (stderr, "linking\n");
        w->link = s->weaks;
        s->weaks = w;
      } else {
        if (DEBUG_WEAK)
          fprintf (stderr, "not linking\n");
      }
    }
    /* Store the forwarding pointer in the old object. */
    *((GC_header*)(p - GC_HEADER_SIZE)) = GC_FORWARDED;
    *((objptr*)p) = pointerToObjptr (s->forwardState.back + headerBytes,
                                     s->forwardState.toStart);
    /* Update the back of the queue. */
    s->forwardState.back += size + skip;
    assert (isAligned ((size_t)s->forwardState.back + GC_NORMAL_HEADER_SIZE,
                       s->alignment));
  }
  *opp = *((objptr*)p);
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "forwardObjptr --> *opp = "FMTPTR"\n",
             (uintptr_t)*opp);
  assert (isObjptrInToSpace (s, *opp));
}

void forwardObjptrIfInNursery (GC_state s, objptr *opp) {
  objptr op;
  pointer p;

  op = *opp;
  p = objptrToPointer (op, s->heap.start);
  if (p < s->heap.nursery)
    return;
  if (DEBUG_GENERATIONAL)
    fprintf (stderr,
             "forwardObjptrIfInNursery  opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR"\n",
             (uintptr_t)opp, op, (uintptr_t)p);
  assert (s->heap.nursery <= p and p < s->limitPlusSlop);
  forwardObjptr (s, opp);
}

/* Walk through all the cards and forward all intergenerational pointers. */
void forwardInterGenerationalObjptrs (GC_state s) {
  GC_cardMapElem *cardMap;
  GC_crossMapElem *crossMap;
  pointer oldGenStart, oldGenEnd;

  size_t cardIndex, maxCardIndex;
  pointer cardStart, cardEnd;
  pointer objectStart;

  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "Forwarding inter-generational pointers.\n");
  updateCrossMap (s);
  /* Constants. */
  cardMap = s->generationalMaps.cardMap;
  crossMap = s->generationalMaps.crossMap;
  maxCardIndex = sizeToCardMapIndex (align (s->heap.oldGenSize, CARD_SIZE));
  oldGenStart = s->heap.start;
  oldGenEnd = oldGenStart + s->heap.oldGenSize;
  /* Loop variables*/
  objectStart = alignFrontier (s, s->heap.start);
  cardIndex = 0;
  cardStart = oldGenStart;
checkAll:
  assert (cardIndex <= maxCardIndex);
  assert (isFrontierAligned (s, objectStart));
  if (cardIndex == maxCardIndex)
    goto done;
checkCard:
  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "checking card %"PRIuMAX"  objectStart = "FMTPTR"\n",
             (uintmax_t)cardIndex, (uintptr_t)objectStart);
  assert (objectStart < oldGenStart + cardMapIndexToSize (cardIndex + 1));
  if (cardMap[cardIndex]) {
    pointer lastObject;

    s->cumulativeStatistics.numCardsMarked++;
    if (DEBUG_GENERATIONAL)
      fprintf (stderr, "card %"PRIuMAX" is marked  objectStart = "FMTPTR"\n",
               (uintmax_t)cardIndex, (uintptr_t)objectStart);
    assert (isFrontierAligned (s, objectStart));
    cardEnd = cardStart + CARD_SIZE;
    if (oldGenEnd < cardEnd)
      cardEnd = oldGenEnd;
    assert (objectStart < cardEnd);
    lastObject = objectStart;
    /* If we ever add Weak.set, then there could be intergenerational
     * weak pointers, in which case we would need to link the weak
     * objects into s->weaks.  But for now, since there is no
     * Weak.set, the foreachObjptrInRange will do the right thing on
     * weaks, since the weak pointer will never be into the nursery.
     */
    objectStart = foreachObjptrInRange (s, objectStart, &cardEnd,
                                        forwardObjptrIfInNursery, FALSE);
    s->cumulativeStatistics.bytesScannedMinor += (uintmax_t)(objectStart - lastObject);
    if (objectStart == oldGenEnd)
      goto done;
    cardIndex = sizeToCardMapIndex ((size_t)(objectStart - oldGenStart));
    cardStart = oldGenStart + cardMapIndexToSize (cardIndex);
    goto checkCard;
  } else {
    unless (CROSS_MAP_EMPTY == crossMap[cardIndex])
      objectStart = cardStart + (size_t)(crossMap[cardIndex] * CROSS_MAP_OFFSET_SCALE);
    if (DEBUG_GENERATIONAL)
      fprintf (stderr,
               "card %"PRIuMAX" is not marked"
               "  crossMap[%"PRIuMAX"] == %"PRIuMAX""
               "  objectStart = "FMTPTR"\n",
               (uintmax_t)cardIndex, (uintmax_t)cardIndex,
               (uintmax_t)(crossMap[cardIndex] * CROSS_MAP_OFFSET_SCALE),
               (uintptr_t)objectStart);
    cardIndex++;
    cardStart += CARD_SIZE;
    goto checkAll;
  }
  assert (FALSE);
done:
  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "Forwarding inter-generational pointers done.\n");
}
