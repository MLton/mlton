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

static inline bool pointerIsInFromSpace (GC_state s, pointer p) {
  return (pointerIsInOldGen (s, p) or pointerIsInNursery (s, p));
}

static inline bool objptrIsInFromSpace (GC_state s, objptr op) {
  return (objptrIsInOldGen (s, op) or objptrIsInNursery (s, op));
}

#if ASSERT
static bool heapHasBytesFree (GC_state s, size_t oldGen, size_t nursery) {
  size_t total;
  bool res;

  total =
    s->heap.oldGenSize + oldGen 
    + (s->canMinor ? 2 : 1) * (s->limitPlusSlop - s->heap.nursery);
  res = 
    (total <= s->heap.size) 
    and (nursery <= (size_t)(s->limitPlusSlop - s->frontier));
  if (DEBUG_DETAILED)
    fprintf (stderr, "%s = hasBytesFree (%zu, %zu)\n",
             boolToString (res),
             /*uintToCommaString*/(oldGen),
             /*uintToCommaString*/(nursery));
  return res;
}
#endif

static inline bool heapIsInit (GC_heap h) {
  return 0 == h->size;
}
