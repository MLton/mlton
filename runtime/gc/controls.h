/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_ratios {
  /* Minimum live ratio to use copying GC. */
  float copy; 
  /* Only use generational GC with copying collection if the ratio of
   * heap size to live data size is below copyGenerational.
   */
  float copyGenerational; 
  float grow; 
  float hashCons;
  /* Desired ratio of heap size to live data. */
  float live; 
  /* Minimum live ratio to us mark-compact GC. */
  float markCompact; 
  /* Only use generational GC with mark-compact collection if the
   * ratio of heap size to live data size is below
   * markCompactGenerational.
   */
  float markCompactGenerational; 
  /* As long as the ratio of bytes live to nursery size is greater
   * than nurseryRatio, use minor GCs.
   */
  float nursery; 
  float ramSlop;
  float threadShrink; 
};

struct GC_controls {
  size_t fixedHeap; /* If 0, then no fixed heap. */
  size_t maxHeap; /* if zero, then unlimited, else limit total heap */
  bool mayLoadWorld;
  bool mayProcessAtMLton;
  bool messages; /* Print a message at the start and end of each gc. */
  size_t oldGenArraySize; /* Arrays larger are allocated in old gen, if possible. */
  struct GC_ratios ratios;
  bool rusageMeasureGC;
  bool summary; /* Print a summary of gc info when program exits. */
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool detailedGCTime (GC_state s);
static inline bool needGCTime (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
