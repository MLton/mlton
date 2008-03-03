/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          translateHeap                           */
/* ---------------------------------------------------------------- */

struct translateState {
  pointer from;
  pointer to;
};
static struct translateState translateState;

void translateObjptr (__attribute__ ((unused)) GC_state s, 
                      objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, translateState.from);
  p = (p - translateState.from) + translateState.to;
  *opp = pointerToObjptr (p, translateState.to);
}

/* translateHeap (s, from, to, size)
 */
void translateHeap (GC_state s, pointer from, pointer to, size_t size) {
  pointer limit;

  if (DEBUG or s->controls->messages)
    fprintf (stderr, "[GC: Translating heap at "FMTPTR" of size %s bytes to "FMTPTR".]\n",
             (uintptr_t)from,
             uintmaxToCommaString(size),
             (uintptr_t)to);
  if (from == to)
    return;
  else {
    translateState.from = from;
    translateState.to = to;
  }
  /* Translate globals and heap. */
  foreachGlobalObjptr (s, translateObjptr);
  limit = to + size;
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, translateObjptr, FALSE);
}
