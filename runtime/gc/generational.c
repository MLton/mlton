/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#define CROSS_MAP_EMPTY ((GC_crossMapElem)255)

static inline uintptr_t pointerToCardIndex (pointer p) {
  return (uintptr_t)p >> CARD_SIZE_LOG2;
}
static inline size_t sizeToCardIndex (size_t n) {
  return n >> CARD_SIZE_LOG2;
}
static inline size_t cardIndexToSize (size_t n) {
  return n << CARD_SIZE_LOG2;
}

static inline pointer pointerToCardMapAddr (GC_state s, pointer p) {
  pointer res;
  
  res = &s->generational.cardMapAbsolute [pointerToCardIndex (p)];
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "pointerToCardMapAddr ("FMTPTR") = "FMTPTR"\n",
             (uintptr_t)p, (uintptr_t)res);
  return res;
}

static inline bool cardIsMarked (GC_state s, pointer p) {
  return (*pointerToCardMapAddr (s, p) != 0x0);
}

static inline void markCard (GC_state s, pointer p) {
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "markCard ("FMTPTR")\n", (uintptr_t)p);
  if (s->mutatorMarksCards)
    *pointerToCardMapAddr (s, p) = 0x1;
}

static inline void clearCardMap (GC_state s) {
  if (DEBUG_GENERATIONAL and DEBUG_DETAILED)
    fprintf (stderr, "clearCardMap ()\n");
  memset (s->generational.cardMap, 0, 
          s->generational.cardMapLength * CARD_MAP_ELEM_SIZE);
}

static inline void clearCrossMap (GC_state s) {
  if (DEBUG_GENERATIONAL and DEBUG_DETAILED)
    fprintf (stderr, "clearCrossMap ()\n");
  s->generational.crossMapValidSize = 0;
  memset (s->generational.crossMap, CROSS_MAP_EMPTY, 
          s->generational.crossMapLength * CROSS_MAP_ELEM_SIZE);
}

static inline void setCardMapAbsolute (GC_state s) {
  unless (s->mutatorMarksCards)
    return;
  /* It's OK if the subtraction below underflows because all the
   * subsequent additions to mark the cards will overflow and put us
   * in the right place.
   */
  s->generational.cardMapAbsolute = 
    pointerToCardMapAddr (s, s->heap.start);
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "cardMapAbsolute = "FMTPTR"\n",
             (uintptr_t)s->generational.cardMapAbsolute);
}

static inline void createCardMapAndCrossMap (GC_state s) {
  unless (s->mutatorMarksCards) {
    s->generational.cardMapLength = 0;
    s->generational.cardMap = NULL;
    s->generational.cardMapAbsolute = NULL;
    s->generational.crossMapLength = 0;
    s->generational.crossMap = NULL;
    return;
  }
  assert (isAligned (s->heap.size, CARD_SIZE));

  size_t cardMapLength, cardMapSize;
  size_t crossMapLength, crossMapSize;
  size_t totalMapSize;

  cardMapLength = sizeToCardIndex (s->heap.size);
  cardMapSize = align (cardMapLength * CARD_MAP_ELEM_SIZE, s->pageSize);
  cardMapLength = cardMapSize / CARD_MAP_ELEM_SIZE;
  s->generational.cardMapLength = cardMapLength;

  crossMapLength = sizeToCardIndex (s->heap.size);
  crossMapSize = align (crossMapLength * CROSS_MAP_ELEM_SIZE, s->pageSize);
  crossMapLength = crossMapSize / CROSS_MAP_ELEM_SIZE;
  s->generational.crossMapLength = crossMapLength;

  totalMapSize = cardMapSize + crossMapSize;
  if (DEBUG_MEM)
    fprintf (stderr, "Creating card/cross map of size %zd\n",
             /*uintToCommaString*/(totalMapSize));
  s->generational.cardMap = 
    GC_mmapAnon (totalMapSize);
  s->generational.crossMap = 
    (GC_crossMapElem*)((pointer)s->generational.cardMap + cardMapSize);
  if (DEBUG_CARD_MARKING)
    fprintf (stderr, "cardMap = "FMTPTR"  crossMap = "FMTPTR"\n",
             (uintptr_t)s->generational.cardMap,
             (uintptr_t)s->generational.crossMap);
  setCardMapAbsolute (s);
  clearCardMap (s);
  clearCrossMap (s);
}

#if ASSERT

static inline pointer crossMapCardStart (GC_state s, pointer p) {
  /* The p - 1 is so that a pointer to the beginning of a card falls
   * into the index for the previous crossMap entry.
   */
  return (p == s->heap.start)
    ? s->heap.start
    : (p - 1) - ((uintptr_t)(p - 1) % CARD_SIZE);
}

/* crossMapIsOK is a slower, but easier to understand, way of
 * computing the crossMap.  updateCrossMap (below) incrementally
 * updates the crossMap, checking only the part of the old generation
 * that it hasn't seen before.  crossMapIsOK simply walks through the
 * entire old generation.  It is useful to check that the incremental
 * update is working correctly.
 */

static inline bool crossMapIsOK (GC_state s) {
  static GC_crossMapElem *map;
  size_t mapSize;

  pointer front, back;
  size_t cardIndex;
  pointer cardStart;
  
  if (DEBUG)
    fprintf (stderr, "crossMapIsOK ()\n");
  mapSize = s->generational.crossMapLength * CROSS_MAP_ELEM_SIZE;
  map = GC_mmapAnon (mapSize);
  memset (map, CROSS_MAP_EMPTY, mapSize);
  back = s->heap.start + s->heap.oldGenSize;
  cardIndex = 0;
  front = alignFrontier (s, s->heap.start);
loopObjects:
  assert (front <= back);
  cardStart = crossMapCardStart (s, front);
  cardIndex = sizeToCardIndex (cardStart - s->heap.start);
  map[cardIndex] = (front - cardStart);
  if (front < back) {
    front += objectSize (s, objectData (s, front));
    goto loopObjects;
  }
  for (size_t i = 0; i < cardIndex; ++i)
    assert (map[i] == s->generational.crossMap[i]);
  GC_munmap (map, mapSize);
  return TRUE;
}

#endif /* ASSERT */

static inline void updateCrossMap (GC_state s) {
  size_t cardIndex;
  pointer cardStart, cardEnd;

  pointer nextObject, objectStart;
  pointer oldGenEnd;
  
  if (s->generational.crossMapValidSize == s->heap.oldGenSize)
    goto done;
  oldGenEnd = s->heap.start + s->heap.oldGenSize;
  objectStart = s->heap.start + s->generational.crossMapValidSize;
  if (objectStart == s->heap.start) {
    cardIndex = 0;
    objectStart = alignFrontier (s, objectStart);
  } else
    cardIndex = sizeToCardIndex (objectStart - 1 - s->heap.start);
  cardStart = s->heap.start + cardIndexToSize (cardIndex);
  cardEnd = cardStart + CARD_SIZE;
loopObjects:
  assert (objectStart < oldGenEnd);
  assert ((objectStart == s->heap.start or cardStart < objectStart)
          and objectStart <= cardEnd);
  nextObject = objectStart + objectSize (s, objectData (s, objectStart));
  if (nextObject > cardEnd) {
    /* We're about to move to a new card, so we are looking at the
     * last object boundary in the current card.  
     * Store it in the crossMap.
     */
    size_t offset;
    
    offset = (objectStart - cardStart);
    assert (offset < CROSS_MAP_EMPTY);
    if (DEBUG_GENERATIONAL)
      fprintf (stderr, "crossMap[%zu] = %zu\n",
               cardIndex, offset);
    s->generational.crossMap[cardIndex] = (GC_crossMapElem)offset;
    cardIndex = sizeToCardIndex (nextObject - 1 - s->heap.start);
    cardStart = s->heap.start + cardIndexToSize (cardIndex);
    cardEnd = cardStart + CARD_SIZE;
  }
  objectStart = nextObject;
  if (objectStart < oldGenEnd)
    goto loopObjects;
  assert (objectStart == oldGenEnd);
  s->generational.crossMap[cardIndex] = (GC_crossMapElem)(oldGenEnd - cardStart);
  s->generational.crossMapValidSize = s->heap.oldGenSize;
done:
  assert (s->generational.crossMapValidSize == s->heap.oldGenSize);
  assert (crossMapIsOK (s));
}

static inline void resizeCardMapAndCrossMap (GC_state s) {
  if (s->mutatorMarksCards
      and (s->generational.cardMapLength * CARD_MAP_ELEM_SIZE)
          != align (sizeToCardIndex (s->heap.size), s->pageSize)) {
    GC_cardMapElem *oldCardMap;
    size_t oldCardMapSize;
    GC_crossMapElem *oldCrossMap;
    size_t oldCrossMapSize;
    
    oldCardMap = s->generational.cardMap;
    oldCardMapSize = s->generational.cardMapLength * CARD_MAP_ELEM_SIZE;
    oldCrossMap = s->generational.crossMap;
    oldCrossMapSize = s->generational.crossMapLength * CROSS_MAP_ELEM_SIZE;
    createCardMapAndCrossMap (s);
    GC_memcpy ((pointer)oldCrossMap, (pointer)s->generational.crossMap,
               min (s->generational.crossMapLength * CROSS_MAP_ELEM_SIZE, 
                    oldCrossMapSize));
    if (DEBUG_MEM)
      fprintf (stderr, "Releasing card/cross map.\n");
    GC_munmap (oldCardMap, oldCardMapSize + oldCrossMapSize);
  }
}
