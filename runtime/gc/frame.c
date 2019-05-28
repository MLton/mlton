/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
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

GC_frameInfo getFrameInfoFromFrameIndex (GC_state s, GC_frameIndex frameIndex) {
  GC_frameInfo frameInfo;

  if (DEBUG_DETAILED)
    fprintf (stderr, "frameIndex = "FMTFI"  frameInfosLength = %"PRIu32"\n",
            frameIndex, s->frameInfosLength);
  assert (frameIndex < s->frameInfosLength);
  frameInfo = &(s->frameInfos[frameIndex]);
  assert (frameInfo->size > 0);
  return frameInfo;
}

GC_frameInfo getFrameInfoFromReturnAddress (GC_state s, GC_returnAddress ra) {
  GC_frameInfo frameInfo;
  GC_frameIndex frameIndex;

  frameIndex = getFrameIndexFromReturnAddress (s, ra);
  frameInfo = getFrameInfoFromFrameIndex(s, frameIndex);
  return frameInfo;
}
