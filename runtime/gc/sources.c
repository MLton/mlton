/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

GC_sourceSeqIndex getCachedStackTopFrameSourceSeqIndex (GC_state s) {
  GC_frameIndex i;

  i = getCachedStackTopFrameIndex (s);
  assert(i < s->sourceMaps.frameSourcesLength);
  return s->sourceMaps.frameSources[i];
}

char* getSourceName (GC_state s, GC_sourceIndex i) {
  assert (i < s->sourceMaps.sourcesLength);
  return s->sourceMaps.sourceNames[s->sourceMaps.sources[i].sourceNameIndex];
}

char* GC_sourceName (GC_state s, GC_sourceIndex i) {
  return getSourceName (s, i);
}

#if HAS_TIME_PROFILING

int compareSourceLabels (const void *v1, const void *v2) {
  const struct GC_sourceLabel* l1 = (const struct GC_sourceLabel*)v1;
  const struct GC_sourceLabel* l2 = (const struct GC_sourceLabel*)v2;
  uintptr_t ui1 = (uintptr_t)(l1->label);
  uintptr_t ui2 = (uintptr_t)(l2->label);

  if (ui1 < ui2)
    return -1;
  else if (ui1 == ui2)
    return 0;
  else /* if (ui1 > ui2) */
    return 1;
}

void sortSourceLabels (GC_state s) {
  GC_sourceLabelIndex i;

  /* Sort sourceLabels by address. */
  qsort (s->sourceMaps.sourceLabels, 
         s->sourceMaps.sourceLabelsLength, 
         sizeof (*s->sourceMaps.sourceLabels),
         compareSourceLabels);
  if (0 == s->sourceMaps.sourceLabels[s->sourceMaps.sourceLabelsLength - 1].label)
    die ("Max source label is 0 -- something is wrong.");
  if (DEBUG_SOURCES)
    for (i = 0; i < s->sourceMaps.sourceLabelsLength; i++)
      fprintf (stderr, FMTPTR"  %"PRIu32"\n",
               (uintptr_t)s->sourceMaps.sourceLabels[i].label,
               s->sourceMaps.sourceLabels[i].sourceSeqIndex);
  if (ASSERT)
    for (i = 1; i < s->sourceMaps.sourceLabelsLength; i++)
      assert (s->sourceMaps.sourceLabels[i-1].label
              <= s->sourceMaps.sourceLabels[i].label);
}

void initTextSources (GC_state s) {
  GC_sourceLabelIndex i;
  code_pointer p;
  GC_sourceSeqIndex sourceSeqIndex;

  sortSourceLabels (s);
  /* Initialize s->sourceMaps.textSources. */
  s->sourceMaps.textEnd = GC_getTextEnd();
  s->sourceMaps.textStart = GC_getTextStart();
  if (ASSERT)
    for (i = 0; i < s->sourceMaps.sourceLabelsLength; i++) {
      code_pointer label;

      label = s->sourceMaps.sourceLabels[i].label;
      assert (0 == label
              or (s->sourceMaps.textStart <= label
                  and label < s->sourceMaps.textEnd));
    }
  s->sourceMaps.textSources =
    (GC_sourceSeqIndex *)
    (calloc_safe((size_t)(s->sourceMaps.textEnd - s->sourceMaps.textStart), 
                 sizeof(*(s->sourceMaps.textSources))));
  p = s->sourceMaps.textStart;
  sourceSeqIndex = SOURCE_SEQ_UNKNOWN;
  for (i = 0; i < s->sourceMaps.sourceLabelsLength; i++) {
    for ( ; p < s->sourceMaps.sourceLabels[i].label; p++)
      s->sourceMaps.textSources[p - s->sourceMaps.textStart] = sourceSeqIndex;
    sourceSeqIndex = s->sourceMaps.sourceLabels[i].sourceSeqIndex;
  }
  for ( ; p < s->sourceMaps.textEnd; p++)
    s->sourceMaps.textSources[p - s->sourceMaps.textStart] = sourceSeqIndex;
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
