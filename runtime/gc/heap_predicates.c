/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool isPointerInHeap (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap->start <= p 
              and p < s->heap->start + s->heap->size));
}

bool isPointerInOldGen (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap->start <= p 
              and p < s->heap->start + s->heap->oldGenSize));
}

bool isPointerInNursery (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap->nursery <= p and p < s->heap->frontier));
}

bool isPointerInFromSpace (GC_state s, pointer p) {
  return (isPointerInOldGen (s, p) 
          or isPointerInNursery (s, p));
}

bool isObjptrInHeap (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap->start);
  return isPointerInHeap (s, p);
}

bool isObjptrInOldGen (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap->start);
  return isPointerInOldGen (s, p);
}

bool isObjptrInNursery (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap->start);
  return isPointerInNursery (s, p);
}

bool isObjptrInFromSpace (GC_state s, objptr op) {
  return (isObjptrInOldGen (s, op) 
          or isObjptrInNursery (s, op));
}

/* Is there space in the heap for "oldGen" additional bytes;
  also, can "nursery" bytes be allocated by the current thread
  without using/claiming any shared resources */
bool hasHeapBytesFree (GC_state s, size_t oldGen, size_t nursery) {
  size_t total;
  bool res;

  total =
    s->heap->oldGenSize + oldGen 
    + (s->canMinor ? 2 : 1) * (s->heap->frontier - s->heap->nursery);
  res = 
    (total <= s->heap->availableSize) 
    and (s->heap->start + s->heap->oldGenSize + oldGen <= s->heap->nursery)
    and (nursery <= (size_t)(s->limitPlusSlop - s->frontier));
  if (DEBUG_DETAILED)
    fprintf (stderr, "%s = hasBytesFree (%s, %s)\n",
             boolToString (res),
             uintmaxToCommaString(oldGen),
             uintmaxToCommaString(nursery));
  return res;
}

bool isHeapInit (GC_heap h) {
  return (0 == h->size);
}
