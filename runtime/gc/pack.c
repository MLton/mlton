/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_pack (GC_state s) {
  size_t keep;

  enter (s);
  if (DEBUG or s->controls.messages)
    fprintf (stderr, 
             "[GC: Packing heap at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(s->heap.start),
             uintmaxToCommaString(s->heap.size));
  /* Could put some code here to skip the GC if there hasn't been much
   * allocated since the last collection.  But you would still need to
   * do a minor GC to make all objects contiguous.
   */
  performGC (s, 0, 0, TRUE, FALSE);
  keep = (size_t)(s->heap.oldGenSize * 1.1);
  if (keep <= s->heap.size) {
    shrinkHeap (s, &s->heap, keep);
    setCardMapAndCrossMap (s);
    setGCStateCurrentHeap (s, 0, 0);
    setGCStateCurrentThreadAndStack (s);
  }
  releaseHeap (s, &s->secondaryHeap);
  if (DEBUG or s->controls.messages)
    fprintf (stderr, 
             "[GC: Packed heap at "FMTPTR" to size %s bytes.]\n",
             (uintptr_t)(s->heap.start),
             uintmaxToCommaString(s->heap.size));
  leave (s);
}

void GC_unpack (GC_state s) {
  enter (s);
  if (DEBUG or s->controls.messages)
    fprintf (stderr, 
             "[GC: Unpacking heap at "FMTPTR" of size %s bytes.]\n", 
             (uintptr_t)(s->heap.start),
             uintmaxToCommaString(s->heap.size));
  /* The enterGC is needed here because minorGC and resizeHeap might
   * move the stack, and the SIGPROF catcher would then see a bogus
   * stack.  The leaveGC has to happen after the setStack.
   */
  enterGC (s);
  minorGC (s);
  resizeHeap (s, s->heap.oldGenSize);
  setCardMapAndCrossMap (s);
  resizeHeapSecondary (s);
  setGCStateCurrentHeap (s, 0, 0);
  setGCStateCurrentThreadAndStack (s);
  leaveGC (s);
  if (DEBUG or s->controls.messages)
    fprintf (stderr, 
             "[GC: Unpacked heap at "FMTPTR" to size %s bytes.]\n",
             (uintptr_t)(s->heap.start),
             uintmaxToCommaString(s->heap.size));
  leave (s);
}
