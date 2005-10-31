/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

struct GC_controls {
  size_t fixedHeap; /* If 0, then no fixed heap. */
  size_t maxHeap; /* if zero, then unlimited, else limit total heap */
  bool mayLoadWorld;
  bool mayProcessAtMLton;
  bool messages; /* Print a message at the start and end of each gc. */
  size_t oldGenArraySize; /* Arrays larger are allocated in old gen, if possible. */
  bool summary; /* Print a summary of gc info when program exits. */
};

bool detailedGCTime (GC_state s);
