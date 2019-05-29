/* Copyright (C) 2019 Matthew Fluet.
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

char* getSourceName (GC_state s, GC_sourceIndex i) {
  assert (i < s->sourceMaps.sourcesLength);
  return s->sourceMaps.sourceNames[s->sourceMaps.sources[i].sourceNameIndex];
}

char* GC_sourceName (GC_state s, GC_sourceIndex i) {
  return getSourceName (s, i);
}

#if HAS_TIME_PROFILING

int compareProfileLabelInfos (const void *v1, const void *v2) {
  const struct GC_profileLabelInfo* l1 = (const struct GC_profileLabelInfo*)v1;
  const struct GC_profileLabelInfo* l2 = (const struct GC_profileLabelInfo*)v2;
  uintptr_t ui1 = (uintptr_t)(l1->profileLabel);
  uintptr_t ui2 = (uintptr_t)(l2->profileLabel);

  if (ui1 < ui2)
    return -1;
  else if (ui1 == ui2)
    return 0;
  else /* if (ui1 > ui2) */
    return 1;
}

void sortProfileLabelInfos (GC_state s) {
  GC_profileLabelInfoIndex i;

  /* Sort profileLabelInfos by address. */
  qsort (s->sourceMaps.profileLabelInfos,
         s->sourceMaps.profileLabelInfosLength,
         sizeof (*s->sourceMaps.profileLabelInfos),
         compareProfileLabelInfos);
  if (0 == s->sourceMaps.profileLabelInfos[s->sourceMaps.profileLabelInfosLength - 1].profileLabel)
    die ("Max source label is 0 -- something is wrong.");
  if (ASSERT)
    for (i = 1; i < s->sourceMaps.profileLabelInfosLength; i++)
      assert (s->sourceMaps.profileLabelInfos[i-1].profileLabel
              <= s->sourceMaps.profileLabelInfos[i].profileLabel);
}

void compressProfileLabelInfos (GC_state s) {
  GC_profileLabelInfoIndex in, out, i;
  GC_sourceSeqIndex sourceSeqIndex;
  
  /* Eliminate duplicate profileLabelInfos */
  out = 0;
  sourceSeqIndex = SOURCE_SEQ_UNKNOWN;
  for (in = 0; in < s->sourceMaps.profileLabelInfosLength; ++in) {
    if (s->sourceMaps.profileLabelInfos[in].sourceSeqIndex != sourceSeqIndex) {
      s->sourceMaps.profileLabelInfos[out++] = s->sourceMaps.profileLabelInfos[in];
      sourceSeqIndex = s->sourceMaps.profileLabelInfos[in].sourceSeqIndex;
    }
  }
  
  s->sourceMaps.profileLabelInfosLength = out;

  if (DEBUG_SOURCES)
    for (i = 0; i < s->sourceMaps.profileLabelInfosLength; i++)
      fprintf (stderr, FMTPTR"  "FMTSSI"\n",
               (uintptr_t)s->sourceMaps.profileLabelInfos[i].profileLabel,
               s->sourceMaps.profileLabelInfos[i].sourceSeqIndex);
}

void initProfileLabelInfos (GC_state s) {
  sortProfileLabelInfos (s);
  compressProfileLabelInfos (s);
}

#endif

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
    uint32_t *sourceSeq;

    sourceSeq = s->sourceMaps.sourceSeqs[i];
    for (j = 1; j <= sourceSeq[0]; j++)
      fprintf (stdout, "%"PRIu32" ", sourceSeq[j]);
    fprintf (stdout, "\n");
  }
}
