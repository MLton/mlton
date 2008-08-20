/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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
        fprintf (stderr, "\t%"PRIuMAX"  "FMTCME"  "FMTCME"\n",
                 (uintmax_t)i, generational->crossMap[i],
                 CROSS_MAP_OFFSET_SCALE * generational->crossMap[i]);
    fprintf (stderr, "\n");
  }
}

GC_cardMapIndex pointerToCardMapIndexAbsolute (pointer p) {
  return (GC_cardMapIndex)p >> CARD_SIZE_LOG2;
}
GC_cardMapIndex sizeToCardMapIndex (size_t z) {
  return (GC_cardMapIndex)z >> CARD_SIZE_LOG2;
}
size_t cardMapIndexToSize (GC_cardMapIndex i) {
  return (size_t)i << CARD_SIZE_LOG2;
}
GC_cardMapElem *pointerToCardMapAddr (GC_state s, pointer p) {
  GC_cardMapElem *res;

  res = &s->generationalMaps.cardMapAbsolute[pointerToCardMapIndexAbsolute (p)];
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "pointerToCardMapAddr ("FMTPTR") = "FMTPTR"\n",
             (uintptr_t)p, (uintptr_t)res);
  return res;
}

bool isCardMarked (GC_state s, pointer p) {
  return (*pointerToCardMapAddr (s, p) != 0x0);
}

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

pointer getCrossMapCardStart (GC_state s, pointer p) {
  /* The p - 1 is so that a pointer to the beginning of a card falls
   * into the index for the previous crossMap entry.
   */
  return
    (p == s->heap.start)
    ? s->heap.start
    : (p - 1) - ((uintptr_t)(p - 1) % CARD_SIZE);
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

/* Compute the number of bytes that are needed to store the card map and
   cross map at the end of a heap with the given size. */
size_t computeCardMapAndCrossMapSize (GC_state s, size_t size) {
  unless (s->mutatorMarksCards) {
    return 0;
  }
  assert (isAligned (size, CARD_SIZE));

  GC_cardMapIndex cardMapLength;
  size_t cardMapSize;
  GC_crossMapIndex crossMapLength;
  size_t crossMapSize;
  size_t totalMapSize;

  cardMapLength = sizeToCardMapIndex (size);
  cardMapSize = align (cardMapLength * CARD_MAP_ELEM_SIZE, s->sysvals.pageSize);
  cardMapLength = (GC_cardMapIndex)(cardMapSize / CARD_MAP_ELEM_SIZE);

  crossMapLength = sizeToCardMapIndex (size);
  crossMapSize = align (crossMapLength * CROSS_MAP_ELEM_SIZE, s->sysvals.pageSize);
  crossMapLength = (GC_crossMapIndex)(crossMapSize / CROSS_MAP_ELEM_SIZE);

  totalMapSize = cardMapSize + crossMapSize;

  return totalMapSize;
}

static inline void initCardMapAndCrossMap (GC_state s, GC_heap h, size_t size) {
  unless (s->mutatorMarksCards) {
    s->generationalMaps.cardMapLength = 0;
    s->generationalMaps.cardMap = NULL;
    s->generationalMaps.cardMapAbsolute = NULL;
    s->generationalMaps.crossMapLength = 0;
    s->generationalMaps.crossMap = NULL;
    return;
  }
  assert (isAligned (size, CARD_SIZE));

  GC_cardMapIndex cardMapLength;
  size_t cardMapSize;
  GC_crossMapIndex crossMapLength;
  size_t crossMapSize;
  size_t totalMapSize;

  cardMapLength = sizeToCardMapIndex (size);
  cardMapSize = align (cardMapLength * CARD_MAP_ELEM_SIZE, s->sysvals.pageSize);
  cardMapLength = (GC_cardMapIndex)(cardMapSize / CARD_MAP_ELEM_SIZE);
  s->generationalMaps.cardMapLength = cardMapLength;

  crossMapLength = sizeToCardMapIndex (size);
  crossMapSize = align (crossMapLength * CROSS_MAP_ELEM_SIZE, s->sysvals.pageSize);
  crossMapLength = (GC_crossMapIndex)(crossMapSize / CROSS_MAP_ELEM_SIZE);
  s->generationalMaps.crossMapLength = crossMapLength;

  totalMapSize = cardMapSize + crossMapSize;
  /* The card map starts at the end of the heap. */
  s->generationalMaps.cardMap =
    (GC_cardMap) (h->start + size);
  s->generationalMaps.crossMap =
    (GC_crossMap) (s->generationalMaps.cardMap + (cardMapSize / CARD_MAP_ELEM_SIZE));
  if (DEBUG_MEM or s->controls.messages)
    fprintf (stderr,
             "[GC: Created card/cross map at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(s->generationalMaps.cardMap),
             uintmaxToCommaString(totalMapSize));
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "cardMap = "FMTPTR"  crossMap = "FMTPTR"\n",
             (uintptr_t)s->generationalMaps.cardMap,
             (uintptr_t)s->generationalMaps.crossMap);
  setCardMapAbsolute (s);
}

void createCardMapAndCrossMap (GC_state s) {
  initCardMapAndCrossMap (s, &s->heap, s->heap.size);
  if (s->mutatorMarksCards) {
    clearCardMap (s);
    clearCrossMap (s);
  }
}

/* This function is called before the given heap becomes the new current heap
   used to store the program datas.
   The 2 heaps can have a different size. */
void copyCardMapAndCrossMap (GC_state s, GC_heap h) {
  if (s->mutatorMarksCards) {
    GC_cardMap oldCardMap;
    size_t oldCardMapSize;
    GC_crossMap oldCrossMap;
    size_t oldCrossMapSize;

    oldCardMap = s->generationalMaps.cardMap;
    oldCardMapSize = s->generationalMaps.cardMapLength * CARD_MAP_ELEM_SIZE;
    oldCrossMap = s->generationalMaps.crossMap;
    oldCrossMapSize = s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE;

    initCardMapAndCrossMap (s, h, h->size);

    clearCardMap (s);
    GC_memcpy ((pointer)oldCardMap, (pointer)s->generationalMaps.cardMap,
               min (s->generationalMaps.cardMapLength * CARD_MAP_ELEM_SIZE,
                    oldCardMapSize));
    clearCrossMap (s);
    GC_memcpy ((pointer)oldCrossMap, (pointer)s->generationalMaps.crossMap,
               min (s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE,
                    oldCrossMapSize));
  }
}

/* This function is called before we shrink the heap buffer which contains the
   card/cross map datas. */
void shrinkCardMapAndCrossMap (GC_state s, size_t keep) {
  if (s->mutatorMarksCards) {
    GC_crossMap oldCrossMap;

    oldCrossMap = s->generationalMaps.crossMap;

    initCardMapAndCrossMap (s, &s->heap, keep);

    GC_memmove ((pointer)oldCrossMap, (pointer)s->generationalMaps.crossMap,
                s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE);
    clearCardMap (s);
  }
}

/* This function is called after we remap the heap buffer which contains the
   card/cross map datas.
   The remapped heap must be bigger than the original one. */
void remapCardMapAndCrossMap (GC_state s, pointer orig) {
  if (s->mutatorMarksCards) {
    GC_cardMap oldCardMap;
    size_t oldCardMapSize;
    GC_crossMap oldCrossMap;
    size_t oldCrossMapSize;

    oldCardMap = (GC_cardMap) ((pointer) s->generationalMaps.cardMap + (s->heap.start - orig));
    oldCardMapSize = s->generationalMaps.cardMapLength * CARD_MAP_ELEM_SIZE;
    oldCrossMap = (GC_crossMap) (oldCardMap + (oldCardMapSize / CARD_MAP_ELEM_SIZE));
    oldCrossMapSize = s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE;

    initCardMapAndCrossMap (s, &s->heap, s->heap.size);

    if (DEBUG_MEM or s->controls.messages) {
      fprintf (stderr, "[GC: oldCardMap = "FMTPTR"  oldCrossMap = "FMTPTR"]\n",
               (uintptr_t)oldCardMap,
               (uintptr_t)oldCrossMap);
      fprintf (stderr,
               "[GC: oldCardMapSize = %s bytes  oldCrossMapSize = %s bytes]\n",
               uintmaxToCommaString(oldCardMapSize), uintmaxToCommaString(oldCrossMapSize));
      fprintf (stderr, "[GC: cardMap = "FMTPTR"  crossMap = "FMTPTR"]\n",
               (uintptr_t)s->generationalMaps.cardMap,
               (uintptr_t)s->generationalMaps.crossMap);
    }
    GC_memmove ((pointer)oldCrossMap, (pointer)s->generationalMaps.crossMap,
                oldCrossMapSize);
    if (DEBUG_MEM or s->controls.messages) {
      fprintf (stderr,
               "[GC: crossMapSize = %s bytes]\n",
               uintmaxToCommaString(s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE));
    }
    memset((pointer)s->generationalMaps.crossMap + oldCrossMapSize, CROSS_MAP_EMPTY,
           s->generationalMaps.crossMapLength * CROSS_MAP_ELEM_SIZE - oldCrossMapSize);
    if (DEBUG_MEM or s->controls.messages) {
      fprintf(stderr, "[GC: cross map OK]\n");
    }
    clearCardMap (s);
    if (DEBUG_MEM or s->controls.messages) {
      fprintf(stderr, "[GC: card map OK]\n");
    }
  }
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
  cardIndex = 0;
  front = alignFrontier (s, s->heap.start);
loopObjects:
  assert (front <= back);
  cardStart = getCrossMapCardStart (s, front);
  cardIndex = sizeToCardMapIndex (cardStart - s->heap.start);
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
    cardIndex = sizeToCardMapIndex (objectStart - 1 - s->heap.start);
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

    offset = (objectStart - cardStart) / CROSS_MAP_OFFSET_SCALE;
    assert (offset < CROSS_MAP_EMPTY);
    if (DEBUG_GENERATIONAL)
      fprintf (stderr, "crossMap[%"PRIuMAX"] = %"PRIuMAX"\n",
               (uintmax_t)cardIndex, (uintmax_t)offset);
    s->generationalMaps.crossMap[cardIndex] = (GC_crossMapElem)offset;
    cardIndex = sizeToCardMapIndex (nextObject - 1 - s->heap.start);
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
