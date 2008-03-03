/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

enum {
  SYNC_NONE = 0,
  SYNC_OLD_GEN_ARRAY,
  SYNC_NEW_GEN_ARRAY,
  SYNC_STACK,
  SYNC_HEAP,
  SYNC_FORCE,
  SYNC_PACK,
  SYNC_SAVE_WORLD,
};

struct GC_cumulativeStatistics {
  uintmax_t bytesAllocated;
  uintmax_t bytesFilled; /* i.e. unused gaps */
  uintmax_t bytesCopied;
  uintmax_t bytesCopiedMinor;
  uintmax_t bytesMarkCompacted;

  uintmax_t markedCards; /* Number of marked cards seen during minor GCs. */

  size_t maxBytesLive;
  size_t maxBytesLiveSinceReset;
  size_t maxHeapSizeSeen;
  size_t maxStackSizeSeen;

  uintmax_t minorBytesScanned;

  uintmax_t numLimitChecks;

  uintmax_t syncForOldGenArray;
  uintmax_t syncForNewGenArray;
  uintmax_t syncForStack;
  uintmax_t syncForHeap;
  uintmax_t syncMisc;

  uintmax_t numCopyingGCs;
  uintmax_t numHashConsGCs;
  uintmax_t numMarkCompactGCs;
  uintmax_t numMinorGCs;

  uintmax_t maxPause;
  struct timeval tv_gc; /* time spent in gc */
  struct rusage ru_gcCopy; /* resource usage in major copying gcs. */
  struct rusage ru_gcMarkCompact; /* resource usage in mark-compact gcs. */
  struct rusage ru_gcMinor; /* resource usage in minor gcs. */
  struct rusage ru_thread; /* total resource for thread operations */

  /* use wall clock instead of rusage */
  struct timeval tv_rt; /* total time "inside" runtime (not incl. synch.) */
  struct timeval tv_sync; /* time waiting to synchronize in runtime */
};

struct GC_lastMajorStatistics {
  uintmax_t bytesHashConsed;
  size_t bytesLive; /* Number of bytes live at most recent major GC. */
  GC_majorKind kind;
  uintmax_t numMinorGCs;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */
