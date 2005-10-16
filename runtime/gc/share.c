/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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
    s->cumulativeStatistics.bytesHashConsed = 0;
  // Don't hash cons during the first round of marking.
  total = mark (s, object, MARK_MODE, FALSE);
  s->objectHashTable = newHashTable (s);
  // Hash cons during the second round of marking.
  mark (s, object, UNMARK_MODE, TRUE);
  destroyHashTable (s->objectHashTable);
  if (DEBUG_SHARE or s->controls.messages)
    bytesHashConsedMessage (s, total);
}
