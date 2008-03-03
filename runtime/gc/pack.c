/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_pack (__attribute__ ((unused)) GC_state *gs) {
  size_t keep;
  GC_state s = pthread_getspecific (gcstate_key);
  s->syncReason = SYNC_PACK;
  ENTER0 (s);
  if (DEBUG or s->controls->messages)
    fprintf (stderr, "[GC: Packing heap at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
  /* Could put some code here to skip the GC if there hasn't been much
   * allocated since the last collection.  But you would still need to
   * do a minor GC to make all objects contiguous.
   */
  performGC (s, 0, 0, TRUE, FALSE);
  keep = s->heap->oldGenSize * 1.1;
  if (keep <= s->heap->size) {
    shrinkHeap (s, s->heap, keep);
    setGCStateCurrentHeap (s, 0, 0, true);
    setGCStateCurrentThreadAndStack (s);
  }
  releaseHeap (s, s->secondaryHeap);
  if (DEBUG or s->controls->messages)
    fprintf (stderr, "[GC: Packed heap at "FMTPTR" to size %s bytes.]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
  LEAVE0 (s);
}

void GC_unpack (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->syncReason = SYNC_PACK;
  ENTER0 (s);
  if (DEBUG or s->controls->messages)
    fprintf (stderr, "[GC: Unpacking heap at "FMTPTR" of size %s bytes.]\n", 
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
  /* The enterGC is needed here because minorGC and resizeHeap might
   * move the stack, and the SIGPROF catcher would then see a bogus
   * stack.  The leaveGC has to happen after the setStack.
   */
  enterGC (s);
  minorGC (s);
  resizeHeap (s, s->heap->oldGenSize);
  resizeHeapSecondary (s);
  setGCStateCurrentHeap (s, 0, 0, true);
  setGCStateCurrentThreadAndStack (s);
  leaveGC (s);
  if (DEBUG or s->controls->messages)
    fprintf (stderr, "[GC: Unpacked heap at "FMTPTR" to size %s bytes.]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
  LEAVE0 (s);
}
