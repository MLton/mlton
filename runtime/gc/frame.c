/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline uint32_t getFrameIndex (GC_state s, GC_returnAddress ra) {
  uint32_t res;

  res = s->returnAddressToFrameIndex (ra);
  if (DEBUG_DETAILED)
    fprintf (stderr, "%"PRIu32" = getFrameIndex ("FMTRA")\n",
             res, ra);
  return res;
}

static inline GC_frameLayout * getFrameLayout (GC_state s, GC_returnAddress ra) {
  GC_frameLayout *layout;
  uint32_t index;

  index = getFrameIndex (s, ra);
  if (DEBUG_DETAILED)
    fprintf (stderr, 
             "returnAddress = "FMTRA
             "  index = %"PRIx32
             "  frameLayoutsSize = %"PRIu16"\n",
             ra, index, s->frameLayoutsSize);
  assert (0 <= index and index < s->frameLayoutsSize);
  layout = &(s->frameLayouts[index]);
  assert (layout->numBytes > 0);
  return layout;
}
