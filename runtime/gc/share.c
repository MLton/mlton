/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_share (GC_state s, pointer object) {
  size_t bytesExamined;
  size_t bytesHashConsed;
  struct GC_markState markState;

  enter (s); /* update stack in heap, in case it is reached */
  if (DEBUG_SHARE)
    fprintf (stderr, "GC_share "FMTPTR"\n", (uintptr_t)object);
  if (DEBUG_SHARE or s->controls.messages)
    s->lastMajorStatistics.bytesHashConsed = 0;
  // Don't hash cons during the first round of marking.
  markState.mode = MARK_MODE;
  markState.size = 0;
  markState.shouldHashCons = FALSE;
  markState.shouldLinkWeaks = FALSE;
  dfsMark (s, object, &markState);
  bytesExamined = markState.size;
  s->objectHashTable = allocHashTable (s);
  // Hash cons during the second round of (un)marking.
  markState.mode = UNMARK_MODE;
  markState.size = 0;
  markState.shouldHashCons = TRUE;
  dfsMark (s, object, &markState);
  freeHashTable (s->objectHashTable);
  bytesHashConsed = s->lastMajorStatistics.bytesHashConsed;
  s->cumulativeStatistics.bytesHashConsed += bytesHashConsed;
  if (DEBUG_SHARE or s->controls.messages)
    printBytesHashConsedMessage (bytesHashConsed, bytesExamined);
  leave (s);
}
