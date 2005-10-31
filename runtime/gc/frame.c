/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

GC_frameIndex getFrameIndexFromReturnAddress (GC_state s, GC_returnAddress ra) {
  GC_frameIndex res;

  res = s->returnAddressToFrameIndex (ra);
  if (DEBUG_DETAILED)
    fprintf (stderr, FMTFI" = getFrameIndexFromReturnAddress ("FMTRA")\n",
             res, ra);
  return res;
}

GC_frameLayout getFrameLayoutFromFrameIndex (GC_state s, GC_frameIndex index) {
  GC_frameLayout layout;

  if (DEBUG_DETAILED)
    fprintf (stderr, "index = "FMTFI"  frameLayoutsLength = %"PRIu32"\n",
            index, s->frameLayoutsLength);
  assert (index < s->frameLayoutsLength);
  layout = &(s->frameLayouts[index]);
  assert (layout->size > 0);
  return layout;
}

GC_frameLayout getFrameLayoutFromReturnAddress (GC_state s, GC_returnAddress ra) {
  GC_frameLayout layout;
  GC_frameIndex index;
  
  index = getFrameIndexFromReturnAddress (s, ra);
  layout = getFrameLayoutFromFrameIndex(s, index);
  return layout;
}
