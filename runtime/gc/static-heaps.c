/* Copyright (C) 2019-2020 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

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

bool isPointerInImmutableMutableOrRootStaticHeap (GC_state s, pointer p) {
  return (isPointerInImmutableStaticHeap (s, p)
          or isPointerInMutableStaticHeap (s, p)
          or isPointerInRootStaticHeap (s, p));
}

bool isObjptrInImmutableMutableOrRootStaticHeap (GC_state s, objptr op) {
  return (isObjptrInImmutableStaticHeap (s, op)
          or isObjptrInMutableStaticHeap (s, op)
          or isObjptrInRootStaticHeap (s, op));
}
