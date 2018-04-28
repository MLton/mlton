/* Copyright (C) 2010 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          translateHeap                           */
/* ---------------------------------------------------------------- */

void translateObjptr (GC_state s, 
                      objptr *opp) {
  pointer p;
  pointer from, to;

  from = s->translateState.from;
  to = s->translateState.to;
  p = objptrToPointer (*opp, from);
  p = (p - from) + to;
  *opp = pointerToObjptr (p, to);
}

/* translateHeap (s, from, to, size)
 */
void translateHeap (GC_state s, pointer from, pointer to, size_t size) {
  pointer limit;

  if (from == to)
    return;

  if (DEBUG or s->controls.messages)
    fprintf (stderr, 
             "[GC: Translating old-gen of size %s bytes of heap at "FMTPTR" from "FMTPTR".]\n",
             uintmaxToCommaString(size),
             (uintptr_t)to,
             (uintptr_t)from);
  s->translateState.from = from;
  s->translateState.to = to;
  /* Translate globals and heap. */
  foreachGlobalObjptr (s, translateObjptr);
  limit = to + size;
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, translateObjptr, FALSE);
}
