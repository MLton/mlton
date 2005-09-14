/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline uint32_t 
getFrameIndexFromReturnAddress (GC_state s, GC_returnAddress ra) {
  uint32_t res;

  res = s->returnAddressToFrameIndex (ra);
  if (DEBUG_DETAILED)
    fprintf (stderr, "%"PRIu32" = getFrameIndex ("FMTRA")\n",
             res, ra);
  return res;
}

static inline GC_frameLayout * 
getFrameLayoutFromFrameIndex (GC_state s, uint32_t index) {
  GC_frameLayout *layout;

  if (DEBUG_DETAILED)
    fprintf (stderr, 
             "index = %"PRIx32
             "  frameLayoutsLength = %"PRIu32"\n",
            index, s->frameLayoutsLength);
  assert (index < s->frameLayoutsLength);
  layout = &(s->frameLayouts[index]);
  assert (layout->size > 0);
  return layout;
}

static inline GC_frameLayout * 
getFrameLayoutFromReturnAddress (GC_state s, GC_returnAddress ra) {
  GC_frameLayout *layout;
  uint32_t index;
  
  index = getFrameIndexFromReturnAddress (s, ra);
  layout = getFrameLayoutFromFrameIndex(s, index);
  return layout;
}
