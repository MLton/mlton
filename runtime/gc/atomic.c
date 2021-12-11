/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void beginAtomic (GC_state s) {
  s->atomicState++;
  if (0 == s->limit)
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
}

void endAtomic (GC_state s) {
  s->atomicState--;
  if (0 == s->atomicState 
      and s->signalsInfo.signalIsPending)
    s->limit = 0;
}
