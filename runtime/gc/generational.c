/* Copyright (C) 2009,2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void displayGenerationalMaps (__attribute__ ((unused)) GC_state s,
                              struct GC_generationalMaps *generational,
                              FILE *stream) {
  fprintf(stream,
          "\t\tcardMap = "FMTPTR"\n"
          "\t\tcardMapAbsolute = "FMTPTR"\n"
          "\t\tcardMapLength = %"PRIuMAX"\n"
          "\t\tcrossMap = "FMTPTR"\n"
          "\t\tcrossMapLength = %"PRIuMAX"\n"
          "\t\tcrossMapValidSize = %"PRIuMAX"\n",
          (uintptr_t)generational->cardMap,
          (uintptr_t)generational->cardMapAbsolute,
          (uintmax_t)generational->cardMapLength,
          (uintptr_t)generational->crossMap,
          (uintmax_t)generational->crossMapLength,
          (uintmax_t)generational->crossMapValidSize);
  if (DEBUG_GENERATIONAL and DEBUG_DETAILED) {
    GC_crossMapIndex i;

    fprintf (stderr, "crossMap trues\n");
    for (i = 0; i < generational->crossMapLength; i++)
      unless (CROSS_MAP_EMPTY == generational->crossMap[i])
        fprintf (stderr, "\t%"PRIuMAX"  "FMTCME"  %"PRIuMAX"\n",
                 (uintmax_t)i, generational->crossMap[i],
                 (uintmax_t)(CROSS_MAP_OFFSET_SCALE * generational->crossMap[i]));
    fprintf (stderr, "\n");
  }
}

GC_cardMapIndex sizeToCardMapIndex (size_t z) {
  return (GC_cardMapIndex)z >> CARD_SIZE_LOG2;
}
size_t cardMapIndexToSize (GC_cardMapIndex i) {
  return (size_t)i << CARD_SIZE_LOG2;
}
GC_cardMapIndex pointerToCardMapIndexAbsolute (pointer p) {
  return (GC_cardMapIndex)p >> CARD_SIZE_LOG2;
}
GC_cardMapElem *pointerToCardMapAddr (GC_state s, pointer p) {
  GC_cardMapElem *res;

  res = &s->generationalMaps.cardMapAbsolute[pointerToCardMapIndexAbsolute (p)];
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "pointerToCardMapAddr ("FMTPTR") = "FMTPTR"\n",
             (uintptr_t)p, (uintptr_t)res);
  return res;
}

GC_crossMapIndex sizeToCrossMapIndex (size_t z) {
  return (GC_crossMapIndex)z >> CARD_SIZE_LOG2;
}

#if ASSERT
bool isCardMarked (GC_state s, pointer p) {
  return (*pointerToCardMapAddr (s, p) != 0x0);
}
#endif

void markCard (GC_state s, pointer p) {
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "markCard ("FMTPTR")\n", (uintptr_t)p);
  if (s->mutatorMarksCards)
    *(pointerToCardMapAddr (s, p)) = 0x1;
}

void markIntergenerationalPointer (GC_state s, pointer *pp) {
  if (s->mutatorMarksCards
      and isPointerInOldGen (s, (pointer)pp)
      and isPointerInNursery (s, *pp))
    markCard (s, (pointer)pp);
}

void markIntergenerationalObjptr (GC_state s, objptr *opp) {
  if (s->mutatorMarksCards
      and isPointerInOldGen (s, (pointer)opp)
      and isObjptrInNursery (s, *opp))
    markCard (s, (pointer)opp);
}

void setCardMapAbsolute (GC_state s) {
  unless (s->mutatorMarksCards)
    return;
  /* It's OK if the subtraction below underflows because all the
   * subsequent additions to mark the cards will overflow and put us
   * in the right place.
   */
  s->generationalMaps.cardMapAbsolute =
    s->generationalMaps.cardMap
    - pointerToCardMapIndexAbsolute (s->heap.start);
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "setCardMapAbsolute = "FMTPTR"\n",
             (uintptr_t)s->generationalMaps.cardMapAbsolute);
}

#if ASSERT
pointer getCrossMapCardStart (GC_state s, pointer p) {
  /* The p - 1 is so that a pointer to the beginning of a card falls
   * into the index for the previous crossMap entry.
   */
  return
    (p == s->heap.start)
    ? s->heap.start
    : (p - 1) - ((uintptr_t)(p - 1) % CARD_SIZE);
}
#endif

size_t sizeofCardMap (GC_state s, size_t heapSize) {
  unless (s->mutatorMarksCards) {
    return 0;
  }
  assert (isAligned (heapSize, CARD_SIZE));

  GC_cardMapIndex cardMapLength;
  size_t cardMapSize;

  cardMapLength = sizeToCardMapIndex (heapSize);
  cardMapSize = align (cardMapLength * CARD_MAP_ELEM_SIZE, s->sysvals.pageSize);

  return cardMapSize;
}

GC_cardMapIndex lenofCardMap (ARG_USED_FOR_ASSERT GC_state s, size_t cardMapSize) {
  GC_cardMapIndex cardMapLength;

  assert (isAligned (cardMapSize, s->sysvals.pageSize));
  assert (isAligned (cardMapSize, CARD_MAP_ELEM_SIZE));

  cardMapLength = (GC_cardMapIndex)(cardMapSize / CARD_MAP_ELEM_SIZE);

  return cardMapLength;
}

size_t sizeofCrossMap (GC_state s, size_t heapSize) {
  unless (s->mutatorMarksCards) {
    return 0;
  }
  assert (isAligned (heapSize, CARD_SIZE));

  GC_crossMapIndex crossMapLength;
  size_t crossMapSize;

  crossMapLength = sizeToCrossMapIndex (heapSize);
  crossMapSize = align (crossMapLength * CROSS_MAP_ELEM_SIZE, s->sysvals.pageSize);

  return crossMapSize;
}

GC_crossMapIndex lenofCrossMap (ARG_USED_FOR_ASSERT GC_state s, size_t crossMapSize) {
  GC_crossMapIndex crossMapLength;

  assert (isAligned (crossMapSize, s->sysvals.pageSize));
  assert (isAligned (crossMapSize, CROSS_MAP_ELEM_SIZE));

  crossMapLength = (GC_crossMapIndex)(crossMapSize / CROSS_MAP_ELEM_SIZE);

  return crossMapLength;
}

void clearCardMap (GC_state s) {
  if (DEBUG_GENERATIONAL and DEBUG_DETAILED)
    fprintf (stderr, "clearCardMap ()\n");
  memset (s->generationalMaps.cardMap, 0,
          s->generationalMaps.cardMapLength * CARD_MAP_ELEM_SIZE);
}

void clearCrossMap (GC_state s) {
  if (DEBUG_GENERATIONAL and DEBUG_DETAILED)
    fprintf (stderr, "clearCrossMap ()\n");
  s->generationalMaps.crossMapValidSize = 0;
  memset (s->generationalMaps.crossMap, CROSS_MAP_EMPTY,
          s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE);
}

void clearCardMapAndCrossMap (GC_state s) {
  clearCardMap (s);
  clearCrossMap (s);
}

size_t sizeofCardMapAndCrossMap (GC_state s, size_t heapSize) {
  size_t totalMapSize;

  totalMapSize = sizeofCardMap (s, heapSize) + sizeofCrossMap (s, heapSize);

  assert (isAligned (totalMapSize, s->sysvals.pageSize));

  return totalMapSize;
}

/*
 * heapSize = invertSizeofCardMapAndCrossMap (s, heapWithMapsSize);
 * implies
 * heapSize + sizeofCardMapAndCrossMap (s, heapSize)
 *  <= heapWithMapsSize
 *  < (heapSize + s->sysvals.pageSize)
 *    + sizeofCardMapAndCrossMap (s, heapSize + s->sysvals.pageSize)
 */
size_t invertSizeofCardMapAndCrossMap (GC_state s, size_t heapWithMapsSize) {
  unless (s->mutatorMarksCards) {
    return heapWithMapsSize;
  }
  assert (isAligned (heapWithMapsSize, s->sysvals.pageSize));

  size_t minHeapSize;
  if (heapWithMapsSize <= 3 * s->sysvals.pageSize) {
    minHeapSize = 0;
  } else {
    double minHeapSizeD;
    minHeapSizeD =
      (((double)(CARD_SIZE)
        / (double)(CARD_SIZE + CARD_MAP_ELEM_SIZE + CROSS_MAP_ELEM_SIZE))
       * (double)(heapWithMapsSize - 3 * s->sysvals.pageSize)) -
      (((double)(CARD_MAP_ELEM_SIZE + CROSS_MAP_ELEM_SIZE)
        / (double)(CARD_SIZE + CARD_MAP_ELEM_SIZE + CROSS_MAP_ELEM_SIZE)) *
       (double)(s->sysvals.pageSize));
    minHeapSize = alignDown ((size_t)minHeapSizeD, s->sysvals.pageSize);
  }

  size_t heapSize = minHeapSize;
  size_t nextHeapSize = heapSize + s->sysvals.pageSize;
  /* The termination condition is:
   *   heapWithMapsSize >= nextHeapSize + sizeofCardMapAndCrossMap (s, nextHeapSize)
   * However, nextHeapSize + sizeofCardMapAndCrossMap (s, nextHeapSize) may overflow.
   */
  while (heapWithMapsSize >= sizeofCardMapAndCrossMap (s, nextHeapSize) and
         heapWithMapsSize - sizeofCardMapAndCrossMap (s, nextHeapSize) >= nextHeapSize) {
    heapSize = nextHeapSize;
    nextHeapSize += s->sysvals.pageSize;
  }

  assert (isAligned (heapSize, s->sysvals.pageSize));
  assert (heapSize + sizeofCardMapAndCrossMap (s, heapSize) <= heapWithMapsSize);
  assert (nextHeapSize == heapSize + s->sysvals.pageSize);
  assert (heapWithMapsSize < sizeofCardMapAndCrossMap (s, nextHeapSize) or
          heapWithMapsSize - sizeofCardMapAndCrossMap (s, nextHeapSize) < nextHeapSize);

  if (DEBUG_DETAILED)
    fprintf (stderr, "invertSizeofCardMapAndCrossMap(%s) = %s\n",
             uintmaxToCommaString(heapWithMapsSize),
             uintmaxToCommaString(heapSize));

  return heapSize;
}

void setCardMapAndCrossMap (GC_state s) {
  unless (s->mutatorMarksCards) {
    s->generationalMaps.cardMapLength = 0;
    s->generationalMaps.cardMap = NULL;
    s->generationalMaps.cardMapAbsolute = NULL;
    s->generationalMaps.crossMapLength = 0;
    s->generationalMaps.crossMap = NULL;
    return;
  }

  GC_cardMapIndex cardMapLength;
  size_t cardMapSize;
  GC_crossMapIndex crossMapLength;
  size_t crossMapSize;

  cardMapSize = sizeofCardMap (s, s->heap.size);
  cardMapLength = lenofCardMap (s, cardMapSize);
  s->generationalMaps.cardMapLength = cardMapLength;

  crossMapSize = sizeofCrossMap (s, s->heap.size);
  crossMapLength = lenofCrossMap (s, crossMapSize);
  s->generationalMaps.crossMapLength = crossMapLength;

  /* The card map starts at the end of the heap. */
  assert (s->heap.withMapsSize == s->heap.size + cardMapSize + crossMapSize);
  s->generationalMaps.cardMap =
    (GC_cardMap) (s->heap.start + s->heap.size);
  s->generationalMaps.crossMap =
    (GC_crossMap) (s->heap.start + s->heap.size + cardMapSize);
  setCardMapAbsolute (s);
  clearCardMapAndCrossMap (s);
}

#if ASSERT
/* isCrossMapOk is a slower, but easier to understand, way of
 * computing the crossMap.  updateCrossMap (below) incrementally
 * updates the crossMap, checking only the part of the old generation
 * that it hasn't seen before.  isCrossMapOk simply walks through the
 * entire old generation.  It is useful to check that the incremental
 * update is working correctly.
 */
bool isCrossMapOk (GC_state s) {
  static GC_crossMapElem *map;
  size_t mapSize;

  pointer front, back;
  GC_cardMapIndex cardIndex;
  pointer cardStart;

  if (DEBUG)
    fprintf (stderr, "isCrossMapOk ()\n");
  mapSize = s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE;
  map = GC_mmapAnon_safe (NULL, mapSize);
  memset (map, CROSS_MAP_EMPTY, mapSize);
  back = s->heap.start + s->heap.oldGenSize;
  front = alignFrontier (s, s->heap.start);
loopObjects:
  assert (front <= back);
  cardStart = getCrossMapCardStart (s, front);
  cardIndex = sizeToCardMapIndex ((size_t)(cardStart - s->heap.start));
  map[cardIndex] = (GC_crossMapElem)((front - cardStart) / CROSS_MAP_OFFSET_SCALE);
  if (front < back) {
    front += sizeofObject (s, advanceToObjectData (s, front));
    goto loopObjects;
  }
  for (size_t i = 0; i < cardIndex; ++i)
    assert (map[i] == s->generationalMaps.crossMap[i]);
  GC_release (map, mapSize);
  return TRUE;
}
#endif

void updateCrossMap (GC_state s) {
  GC_cardMapIndex cardIndex;
  pointer cardStart, cardEnd;

  pointer nextObject, objectStart;
  pointer oldGenEnd;

  if (DEBUG_GENERATIONAL) {
    fprintf (stderr, "updateCrossMap starting\n");
    displayGenerationalMaps (s, &s->generationalMaps, stderr);
  }
  assert (isAligned (s->alignment, CROSS_MAP_OFFSET_SCALE));
  if (s->generationalMaps.crossMapValidSize == s->heap.oldGenSize)
    goto done;
  oldGenEnd = s->heap.start + s->heap.oldGenSize;
  objectStart = s->heap.start + s->generationalMaps.crossMapValidSize;
  if (objectStart == s->heap.start) {
    cardIndex = 0;
    objectStart = alignFrontier (s, objectStart);
  } else
    cardIndex = sizeToCardMapIndex ((size_t)(objectStart - s->heap.start) - 1);
  cardStart = s->heap.start + cardMapIndexToSize (cardIndex);
  cardEnd = cardStart + CARD_SIZE;
loopObjects:
  assert (objectStart < oldGenEnd);
  assert ((objectStart == s->heap.start or cardStart < objectStart)
          and objectStart <= cardEnd);
  nextObject = objectStart + sizeofObject (s, advanceToObjectData (s, objectStart));
  if (DEBUG_GENERATIONAL) {
    fprintf (stderr,
             "\tloopObjects:\n"
             "\t  cardIndex = %"PRIuMAX"\n"
             "\t  cardStart = "FMTPTR"\n"
             "\t    cardEnd = "FMTPTR"\n"
             "\tobjectStart = "FMTPTR"\n"
             "\t nextObject = "FMTPTR"\n",
             (uintmax_t)cardIndex, (uintptr_t)cardStart, (uintptr_t)cardEnd,
             (uintptr_t)objectStart, (uintptr_t)nextObject);
  }
  if (nextObject > cardEnd) {
    /* We're about to move to a new card, so we are looking at the
     * last object boundary in the current card.
     * Store it in the crossMap.
     */
    size_t offset;

    offset = (size_t)(objectStart - cardStart) / CROSS_MAP_OFFSET_SCALE;
    assert (offset < CROSS_MAP_EMPTY);
    if (DEBUG_GENERATIONAL)
      fprintf (stderr, "crossMap[%"PRIuMAX"] = %"PRIuMAX"\n",
               (uintmax_t)cardIndex, (uintmax_t)offset);
    s->generationalMaps.crossMap[cardIndex] = (GC_crossMapElem)offset;
    cardIndex = sizeToCardMapIndex ((size_t)(nextObject - s->heap.start) - 1);
    cardStart = s->heap.start + cardMapIndexToSize (cardIndex);
    cardEnd = cardStart + CARD_SIZE;
  }
  objectStart = nextObject;
  if (objectStart < oldGenEnd)
    goto loopObjects;
  assert (objectStart == oldGenEnd);
  s->generationalMaps.crossMap[cardIndex] =
    (GC_crossMapElem)(oldGenEnd - cardStart) / CROSS_MAP_OFFSET_SCALE;
  s->generationalMaps.crossMapValidSize = s->heap.oldGenSize;
done:
  assert (s->generationalMaps.crossMapValidSize == s->heap.oldGenSize);
  assert (isCrossMapOk (s));
  if (DEBUG_GENERATIONAL) {
    fprintf (stderr, "updateCrossMap finished\n");
    displayGenerationalMaps (s, &s->generationalMaps, stderr);
  }
}
