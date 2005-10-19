/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline void atomicBegin (GC_state s) {
  s->atomicState++;
  if (0 == s->limit)
    s->limit = s->limitPlusSlop - LIMIT_SLOP;
}

static inline void atomicEnd (GC_state s) {
  s->atomicState--;
  if (0 == s->atomicState 
      and s->signalsInfo.signalIsPending)
    s->limit = 0;
}
