/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_share (GC_state s, pointer object) {
  size_t total;

  if (DEBUG_SHARE)
    fprintf (stderr, "GC_share "FMTPTR"\n", (uintptr_t)object);
  if (DEBUG_SHARE or s->controls.messages)
    s->lastMajorStatistics.bytesHashConsed = 0;
  // Don't hash cons during the first round of marking.
  total = dfsMarkByMode (s, object, MARK_MODE, FALSE, FALSE);
  s->objectHashTable = allocHashTable (s);
  // Hash cons during the second round of (un)marking.
  dfsMarkByMode (s, object, UNMARK_MODE, TRUE, FALSE);
  freeHashTable (s->objectHashTable);
  if (DEBUG_SHARE or s->controls.messages)
    printBytesHashConsedMessage (s, total);
}
