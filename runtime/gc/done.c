/* Copyright (C) 2012,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

static void displayCol (FILE *out, size_t width, const char *s) {
  size_t extra;
  size_t i;
  size_t len;

  len = strlen (s);
  if (len < width) {
    extra = width - len;
    for (i = 0; i < extra; i++)
      fprintf (out, " ");
  }
  fprintf (out, "%s\t", s);
}

static void displayCollectionStats (FILE *out, const char *name, struct rusage *ru, 
                                    uintmax_t num, uintmax_t bytes) {
  uintmax_t ms;

  ms = rusageTime (ru);
  fprintf (out, "%s", name);
  displayCol (out, 7, uintmaxToCommaString (ms));
  displayCol (out, 7, uintmaxToCommaString (num));
  displayCol (out, 15, uintmaxToCommaString (bytes));
  displayCol (out, 15, 
              (ms > 0)
              ? uintmaxToCommaString ((uintmax_t)(1000.0 * (float)bytes/(float)ms))
              : "-");
  fprintf (out, "\n");
}

void GC_done (GC_state s) {
  FILE *out;

  enter (s);
  minorGC (s);
  out = s->controls.summaryFile;
  if (s->controls.summary) {
    struct rusage ru_total;
    uintmax_t gcTime;
    uintmax_t totalTime;

    getrusage (RUSAGE_SELF, &ru_total);
    totalTime = rusageTime (&ru_total);
    gcTime = rusageTime (&s->cumulativeStatistics.ru_gc);
    fprintf (out, "GC type\t\ttime ms\t number\t\t  bytes\t      bytes/sec\n");
    fprintf (out, "-------------\t-------\t-------\t---------------\t---------------\n");
    displayCollectionStats
      (out, "copying\t\t", 
       &s->cumulativeStatistics.ru_gcCopying, 
       s->cumulativeStatistics.numCopyingGCs, 
       s->cumulativeStatistics.bytesCopied);
    displayCollectionStats
      (out, "mark-compact\t", 
       &s->cumulativeStatistics.ru_gcMarkCompact, 
       s->cumulativeStatistics.numMarkCompactGCs, 
       s->cumulativeStatistics.bytesMarkCompacted);
    displayCollectionStats
      (out, "minor\t\t",
       &s->cumulativeStatistics.ru_gcMinor, 
       s->cumulativeStatistics.numMinorGCs, 
       s->cumulativeStatistics.bytesCopiedMinor);
    fprintf (out, "total time: %s ms\n",
             uintmaxToCommaString (totalTime));
    fprintf (out, "total GC time: %s ms (%.1f%%)\n",
             uintmaxToCommaString (gcTime), 
             (0 == totalTime) 
             ? 0.0 
             : 100.0 * ((double) gcTime) / (double)totalTime);
    fprintf (out, "max pause time: %s ms\n",
             uintmaxToCommaString (s->cumulativeStatistics.maxPauseTime));
    fprintf (out, "total bytes allocated: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics.bytesAllocated));
    fprintf (out, "max bytes live: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics.maxBytesLive));
    fprintf (out, "max heap size: %s bytes\n", 
             uintmaxToCommaString (s->cumulativeStatistics.maxHeapSize));
    fprintf (out, "max stack size: %s bytes\n", 
             uintmaxToCommaString (s->cumulativeStatistics.maxStackSize));
    fprintf (out, "num cards marked: %s\n", 
             uintmaxToCommaString (s->cumulativeStatistics.numCardsMarked));
    fprintf (out, "bytes scanned: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics.bytesScannedMinor));
    fprintf (out, "bytes hash consed: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics.bytesHashConsed));
  }
  releaseHeap (s, &s->heap);
  releaseHeap (s, &s->secondaryHeap);
}
