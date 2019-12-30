/* Copyright (C) 2012,2017,2019 Matthew Fluet.
 * Copyright (C) 2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

bool isPointerInOldGen (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.start <= p 
              and p <= s->heap.start + s->heap.oldGenSize));
}

bool isPointerInNursery (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.nursery <= p
              and p <= s->frontier));
}

bool isObjptrInOldGen (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return isPointerInOldGen (s, p);
}

bool isObjptrInNursery (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return isPointerInNursery (s, p);
}

bool isPointerInFromSpace (GC_state s, pointer p) {
  return (isPointerInOldGen (s, p)
          or isPointerInNursery (s, p));
}

bool isObjptrInFromSpace (GC_state s, objptr op) {
  return (isObjptrInOldGen (s, op) 
          or isObjptrInNursery (s, op));
}

bool isPointerInHeap (GC_state s, pointer p) {
  return isPointerInFromSpace(s, p);
}

bool isObjptrInHeap (GC_state s, objptr op) {
  return isObjptrInFromSpace(s, op);
}

bool hasHeapBytesFree (GC_state s, size_t oldGen, size_t nursery) {
  size_t total;
  bool res;

  total =
    s->heap.oldGenSize + oldGen 
    + (s->canMinor ? 2 : 1) * (size_t)(s->limitPlusSlop - s->heap.nursery);
  res = 
    (total <= s->heap.size) 
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

bool isPointerInImmutableStaticHeap (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->staticHeaps.immutable.start <= p
              and p <= s->staticHeaps.immutable.start + s->staticHeaps.immutable.size));
}

bool isObjptrInImmutableStaticHeap (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return isPointerInImmutableStaticHeap (s, p);
}

bool isPointerInMutableStaticHeap (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->staticHeaps.mutable.start <= p
              and p <= s->staticHeaps.mutable.start + s->staticHeaps.mutable.size));
}

bool isObjptrInMutableStaticHeap (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return isPointerInMutableStaticHeap (s, p);
}

bool isPointerInRootStaticHeap (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->staticHeaps.root.start <= p
              and p <= s->staticHeaps.root.start + s->staticHeaps.root.size));
}

bool isObjptrInRootStaticHeap (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return isPointerInRootStaticHeap (s, p);
}

bool isPointerInStaticHeap (GC_state s, pointer p) {
  return (isPointerInImmutableStaticHeap (s, p)
          or isPointerInMutableStaticHeap (s, p)
          or isPointerInRootStaticHeap (s, p));
}

bool isObjptrInStaticHeap (GC_state s, objptr op) {
  return (isObjptrInImmutableStaticHeap (s, op)
          or isObjptrInMutableStaticHeap (s, op)
          or isObjptrInRootStaticHeap (s, op));
}
