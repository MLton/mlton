/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* must agree w/ cardSizeLog2 in ssa-to-rssa.fun */
#define CARD_SIZE_LOG2 8
#define CARD_SIZE TWOPOWER(CARD_SIZE_LOG2)

typedef uint8_t GC_cardMapElem;
typedef uint8_t GC_crossMapElem;
typedef size_t GC_cardMapIndex;
typedef size_t GC_crossMapIndex;
#define CARD_MAP_ELEM_SIZE sizeof(GC_cardMapElem)
#define CROSS_MAP_ELEM_SIZE sizeof(GC_crossMapElem)
#define CROSS_MAP_EMPTY ((GC_crossMapElem)255)
#define CROSS_MAP_OFFSET_SCALE 4
#define FMTCMI "%zu"
#define FMTCME "%"PRIu8

struct GC_generationalMaps {
  /* cardMap is an array with cardinality equal to the size of the
   * heap divided by card size.  Each element in the array is
   * interpreted as a boolean; true indicates that some mutable field
   * of some object in the corresponding card in the heap has been
   * written since the last minor GC; hence, the corresponding card
   * must be traced at the next minor GC.
   */
  GC_cardMapElem *cardMap;
  GC_cardMapElem *cardMapAbsolute;
  GC_cardMapIndex cardMapLength;
  /* crossMap is an array with cardinality equal to the size of the
   * heap divided by card size.  Each element in the array is
   * interpreted as a byte offset (scaled by CARD_MAP_OFFSET_SCALE);
   * the offset indicates the start of the last object in the
   * corresponding card from the start of the card.
   */
  GC_crossMapElem *crossMap;
  GC_crossMapIndex crossMapLength;
  /* crossMapValidSize the size of the prefix of the old generation
   * for which the crossMap is valid.
   */
  size_t crossMapValidSize;
};

void displayGenerationalMaps (GC_state s,
                              struct GC_generationalMaps *generational,
                              FILE *stream);

GC_cardMapIndex pointerToCardMapIndexAbsolute (pointer p);
GC_cardMapIndex sizeToCardMapIndex (size_t z);
size_t cardMapIndexToSize (GC_cardMapIndex i);
pointer pointerToCardMapAddr (GC_state s, pointer p);

bool isCardMarked (GC_state s, pointer p);
void markCard (GC_state s, pointer p);

void setCardMapAbsolute (GC_state s);
pointer getCrossMapCardStart (GC_state s, pointer p);

void clearCardMap (GC_state s);
void clearCrossMap (GC_state s);
void createCardMapAndCrossMap (GC_state s);
void resizeCardMapAndCrossMap (GC_state s);

bool isCrossMapOk (GC_state s);
void updateCrossMap (GC_state s);
