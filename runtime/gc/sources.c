/* Copyright (C) 2019,2021 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

GC_sourceSeqIndex getCachedStackTopFrameSourceSeqIndex (GC_state s) {
  GC_frameIndex i;

  i = getCachedStackTopFrameIndex (s);
  assert(i < s->frameInfosLength);
  return s->frameInfos[i].sourceSeqIndex;
}

const char * getSourceName (GC_state s, GC_sourceIndex i) {
  assert (i < s->sourceMaps.sourcesLength);
  return s->sourceMaps.sourceNames[s->sourceMaps.sources[i].sourceNameIndex];
}

const char * GC_sourceName (GC_state s, GC_sourceIndex i) {
  return getSourceName (s, i);
}

void showSources (GC_state s) {
  uint32_t i;
  uint32_t j;

  fprintf (stdout, "0x%08"PRIx32"\n", s->magic);
  fprintf (stdout, "%"PRIu32"\n", s->sourceMaps.sourceNamesLength);
  for (i = 0; i < s->sourceMaps.sourceNamesLength; i++)
    fprintf (stdout, "%s\n", s->sourceMaps.sourceNames[i]);
  fprintf (stdout, "%"PRIu32"\n", s->sourceMaps.sourcesLength);
  for (i = 0; i < s->sourceMaps.sourcesLength; i++)
    fprintf (stdout, "%"PRIu32" %"PRIu32"\n",
             s->sourceMaps.sources[i].sourceNameIndex,
             s->sourceMaps.sources[i].successorSourceSeqIndex);
  fprintf (stdout, "%"PRIu32"\n", s->sourceMaps.sourceSeqsLength);
  for (i = 0; i < s->sourceMaps.sourceSeqsLength; i++) {
    const uint32_t *sourceSeq;

    sourceSeq = s->sourceMaps.sourceSeqs[i];
    for (j = 1; j <= sourceSeq[0]; j++)
      fprintf (stdout, "%"PRIu32" ", sourceSeq[j]);
    fprintf (stdout, "\n");
  }
}
