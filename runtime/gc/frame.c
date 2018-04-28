/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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

GC_frameLayout getFrameLayoutFromFrameIndex (GC_state s, GC_frameIndex findex) {
  GC_frameLayout layout;

  if (DEBUG_DETAILED)
    fprintf (stderr, "findex = "FMTFI"  frameLayoutsLength = %"PRIu32"\n",
            findex, s->frameLayoutsLength);
  assert (findex < s->frameLayoutsLength);
  layout = &(s->frameLayouts[findex]);
  assert (layout->size > 0);
  return layout;
}

GC_frameLayout getFrameLayoutFromReturnAddress (GC_state s, GC_returnAddress ra) {
  GC_frameLayout layout;
  GC_frameIndex findex;

  findex = getFrameIndexFromReturnAddress (s, ra);
  layout = getFrameLayoutFromFrameIndex(s, findex);
  return layout;
}
