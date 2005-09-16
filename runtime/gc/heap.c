/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline bool pointerIsInHeap (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.start <= p 
              and p < s->frontier));
}

static inline bool objptrIsInHeap (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return pointerIsInHeap (s, p);
}

static inline bool pointerIsInOldGen (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.start <= p 
              and p < s->heap.start + s->heap.oldGenSize));
}

static inline bool objptrIsInOldGen (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return pointerIsInOldGen (s, p);
}

static inline bool pointerIsInNursery (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->heap.nursery <= p and p < s->frontier));
}

static inline bool objptrIsInNursery (GC_state s, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap.start);
  return pointerIsInNursery (s, p);
}

void displayHeap (GC_state s,
                  GC_heap heap,
                  FILE *stream) {
  fprintf(stream,
          "\t\tnursery ="FMTPTR"\n"
          "\t\toldGenSize = %zu\n"
          "\t\tstart = "FMTPTR"\n"
          "\t\tsize = %zu\n",
          (uintptr_t)heap->nursery,
          heap->oldGenSize,
          (uintptr_t)heap->start,
          heap->size);
}
