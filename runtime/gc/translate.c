/* Copyright (C) 2010,2019-2020,2024 Matthew Fluet.
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

typedef struct GC_translateState {
  pointer from;
  pointer to;
  size_t size;
} *GC_translateState;

void translateFun (__attribute__((unused)) GC_state s, objptr *opp, void *env) {
  GC_translateState translateState = env;
  pointer p;
  pointer from, to;

  from = translateState->from;
  to = translateState->to;
  p = objptrToPointer (*opp, from);
  if ((from <= p) and
      (p <= from + translateState->size)) {
    p = (p - from) + to;
    *opp = pointerToObjptr (p, to);
  }
}

/* translateHeap (s, from, to, size)
 */
void translateHeap (GC_state s, pointer from, pointer to, size_t size) {
  pointer limit;

  if (from == to)
    return;

  if (DEBUG or s->controls.messages)
    fprintf (stderr, 
             "[GC: Translating %s bytes of heap at "FMTPTR" from "FMTPTR".]\n",
             uintmaxToCommaString(size),
             (uintptr_t)to,
             (uintptr_t)from);
  struct GC_translateState translateState = {.from = from, .to = to, .size = size};
  struct GC_foreachObjptrClosure translateClosure = {.fun = translateFun, .env = &translateState};
  /* Translate globals and heap. */
  foreachGlobalObjptr (s, &translateClosure);
  limit = to + size;
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, &translateClosure, FALSE);
}
