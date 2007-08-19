/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static void displayCol (FILE *out, int width, const char *s) {
  int extra;
  int i;
  int len;

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
              ? uintmaxToCommaString (1000.0 * (float)bytes/(float)ms)
              : "-");
  fprintf (out, "\n");
}

void GC_done (GC_state s) {
  FILE *out;

  enter (s);
  minorGC (s);
  out = stderr;
  if (s->controls.summary) {
    uintmax_t totalTime;
    uintmax_t gcTime;

    gcTime = rusageTime (&s->cumulativeStatistics.ru_gc);
    fprintf (out, "GC type\t\ttime ms\t number\t\t  bytes\t      bytes/sec\n");
    fprintf (out, "-------------\t-------\t-------\t---------------\t---------------\n");
    displayCollectionStats
      (out, "copying\t\t", 
       &s->cumulativeStatistics.ru_gcCopy, 
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
    totalTime = getCurrentTime () - s->startTime;
    fprintf (out, "total GC time: %s ms (%.1f%%)\n",
             uintmaxToCommaString (gcTime), 
             (0 == totalTime) 
             ? 0.0 
             : 100.0 * ((double) gcTime) / (double)totalTime);
    fprintf (out, "max pause: %s ms\n",
             uintmaxToCommaString (s->cumulativeStatistics.maxPause));
    fprintf (out, "total allocated: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics.bytesAllocated));
    fprintf (out, "max live: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics.maxBytesLive));
    fprintf (out, "max semispace: %s bytes\n", 
             uintmaxToCommaString (s->cumulativeStatistics.maxHeapSizeSeen));
    fprintf (out, "max stack size: %s bytes\n", 
             uintmaxToCommaString (s->cumulativeStatistics.maxStackSizeSeen));
    fprintf (out, "marked cards: %s\n", 
             uintmaxToCommaString (s->cumulativeStatistics.markedCards));
    fprintf (out, "minor scanned: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics.minorBytesScanned));
  }
  releaseHeap (s, &s->heap);
  releaseHeap (s, &s->secondaryHeap);
}
