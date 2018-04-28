/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_cumulativeStatistics {
  uintmax_t bytesAllocated;
  uintmax_t bytesCopied;
  uintmax_t bytesCopiedMinor;
  uintmax_t bytesHashConsed;
  uintmax_t bytesMarkCompacted;
  uintmax_t bytesScannedMinor;

  size_t maxBytesLive;
  size_t maxHeapSize;
  uintmax_t maxPauseTime;
  size_t maxStackSize;

  uintmax_t numCardsMarked; /* Number of marked cards seen during minor GCs. */

  uintmax_t numGCs;
  uintmax_t numCopyingGCs;
  uintmax_t numHashConsGCs;
  uintmax_t numMarkCompactGCs;
  uintmax_t numMinorGCs;

  struct rusage ru_gc; /* total resource usage in gc. */
  struct rusage ru_gcCopying; /* resource usage in major copying gcs. */
  struct rusage ru_gcMarkCompact; /* resource usage in major mark-compact gcs. */
  struct rusage ru_gcMinor; /* resource usage in minor copying gcs. */
};

struct GC_lastMajorStatistics {
  size_t bytesHashConsed;
  size_t bytesLive; /* Number of bytes live at most recent major GC. */
  GC_majorKind kind;
  uintmax_t numMinorGCs;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */
